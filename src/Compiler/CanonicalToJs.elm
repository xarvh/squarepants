module Compiler.CanonicalToJs exposing (..)

import Compiler.CoreModule as Core
import Dict exposing (Dict)
import Lib
import Prelude
import RefHierarchy
import Set exposing (Set)
import StateMonad as M exposing (M, do, return)
import Types.CanonicalAst as CA
import Types.Error exposing (ErrorEnv)
import Types.JavascriptAst as JA
import Types.Literal


allNatives : Dict String JA.Name
allNatives =
    Dict.empty
        |> Dict.union (Dict.map (always .fnName) nativeBinops)
        |> Dict.insert "not" "sp_not"
        |> Dict.insert Core.trueValue "true"
        |> Dict.insert Core.falseValue "false"
        |> Dict.insert Core.noneValue "null"
        |> Dict.insert "SPCore/Debug.log" "sp_log"
        |> Dict.insert "SPCore/Debug.todo" "sp_todo"
        |> Dict.insert "SPCore/Debug.toHuman" "sp_toHuman"
        |> Dict.insert "SPCore/Text.fromNumber" "text_fromNumber"
        |> Dict.insert "SPCore/Text.toNumber" "text_toNumber"
        |> Dict.insert "SPCore/Text.split" "text_split"
        |> Dict.insert "SPCore/Text.length" "text_length"
        |> Dict.insert "SPCore/Text.slice" "text_slice"
        |> Dict.insert "SPCore/Text.startsWith" "text_startsWith"
        |> Dict.insert "SPCore/Text.startsWithRegex" "text_startsWithRegex"
        |> Dict.insert "SPCore/Text.replaceRegex" "text_replaceRegex"
        |> Dict.insert "SPCore/Text.trimLeft" "text_trimLeft"
        |> Dict.insert "SPCore/Text.dropLeft" "text_dropLeft"
        |> Dict.insert "SPCore/Text.forEach" "text_forEach"
        |> Dict.insert "SPCore/Basics.modBy" "basics_modBy"
        |> Dict.insert "SPCore/Basics.compare" "basics_compare"
        |> Dict.insert "SPCore/List.sortBy" "list_sortBy"
        |> Dict.insert "/" "sp_divide"
        |> Dict.insert "::" "sp_cons"
        |> Dict.insert "==" "sp_equal"
        |> Dict.insert "/=" "sp_not_equal"
        -- Platform natives
        |> Dict.insert "IO.parallel" "io_parallel"
        |> Dict.insert "IO.readDir" "io_readDir"
        |> Dict.insert "IO.readFile" "io_readFile"
        |> Dict.insert "IO.writeFile" "io_writeFile"
        |> Dict.insert "IO.writeStdout" "io_writeStdout"


nativeUnops : Dict String { jsSymb : JA.Name }
nativeUnops =
    Dict.empty
        |> Dict.insert Prelude.unaryPlus.symbol { jsSymb = "+" }
        |> Dict.insert Prelude.unaryMinus.symbol { jsSymb = "-" }
        |> Dict.insert Prelude.not_.symbol { jsSymb = "!" }


nativeBinops : Dict String { jsSymb : JA.Name, mutates : Bool, fnName : String }
nativeBinops =
    Dict.empty
        -- fnName is used for when the op is used as a function such as `(+)`
        |> Dict.insert "+" { jsSymb = "+", mutates = False, fnName = "add" }
        |> Dict.insert "*" { jsSymb = "*", mutates = False, fnName = "mul" }
        |> Dict.insert "-" { jsSymb = "-", mutates = False, fnName = "sub" }
        |> Dict.insert ":=" { jsSymb = "=", mutates = True, fnName = "mutass" }
        |> Dict.insert "+=" { jsSymb = "+=", mutates = True, fnName = "mutadd" }
        |> Dict.insert ".." { jsSymb = "+", mutates = False, fnName = "strcon" }
        |> Dict.insert ">" { jsSymb = ">", mutates = False, fnName = "greaterThan" }
        |> Dict.insert "<" { jsSymb = "<", mutates = False, fnName = "lesserThan" }
        |> Dict.insert ">=" { jsSymb = ">=", mutates = False, fnName = "greaterOrEqual" }
        |> Dict.insert "<=" { jsSymb = "<=", mutates = False, fnName = "lesserOrEqual" }
        |> Dict.insert "or" { jsSymb = "||", mutates = False, fnName = "or" }
        |> Dict.insert "and" { jsSymb = "&&", mutates = False, fnName = "and" }


none =
    translatePath Core.noneValue


nativeBinopToFunction : String -> { jsSymb : JA.Name, mutates : Bool, fnName : String } -> List JA.Statement -> List JA.Statement
nativeBinopToFunction spName { jsSymb, mutates, fnName } acc =
    let
        d =
            CA.N
    in
    ([ CA.Evaluation
        (CA.Lambda d
            (CA.ParameterPattern <| CA.PatternAny d "a")
            [ CA.Evaluation
                (CA.Lambda d
                    (CA.ParameterPattern <| CA.PatternAny d "b")
                    [ CA.Evaluation
                        (CA.Call d
                            (CA.Call d
                                (CA.Variable d { attrPath = [], isRoot = True, name = spName })
                                (CA.ArgumentExpression
                                    (CA.Variable d { attrPath = [], isRoot = False, name = "a" })
                                )
                            )
                            ((if mutates then
                                CA.ArgumentMutable d

                              else
                                CA.ArgumentExpression << CA.Variable d
                             )
                                { attrPath = [], isRoot = False, name = "b" }
                            )
                        )
                    ]
                )
            ]
        )
     ]
        |> translateBodyToExpr { mutables = Set.empty, errorEnv = { moduleByName = Dict.empty } }
        |> JA.Define fnName
    )
        :: acc


nativeBinopsAsFns =
    Dict.foldl nativeBinopToFunction [] nativeBinops



{---
--- Mutation
--

  CA.Definition: `x @= 0`

     => `const x = { obj : { m : 0 }, attr : 'm' };`



  CA.Variable: `x`

     => `$clone$(x.obj[x.attr])`

     (when we have a non-variable type we can optimize this)



  CA.Call (mutation primitive): `@x += 1`

      CA.ArgumentMutable, fn is primitive: `@x := 0`

        => `x.obj[x.attr] := 0`


      CA.ArgumentMutable, fn is NOT primitive
        if attrPath == []: `doStuff @x`

          => `doStuff(x)`


        else: `doStuff @x.a.b.c`

          => `doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })



-}


type alias Env =
    { mutables : Set JA.Name
    , errorEnv : ErrorEnv
    }


wrapMutable : Bool -> JA.Expr -> JA.Expr
wrapMutable mutable expr =
    if mutable then
        Dict.empty
            |> Dict.insert "attr" (JA.Literal <| quoteAndEscape "$")
            |> Dict.insert "obj" (JA.Record <| Dict.singleton "$" expr)
            |> JA.Record

    else
        expr


unwrapMutable : JA.Name -> JA.Expr
unwrapMutable x =
    let
        ja_x =
            JA.Var x
    in
    -- $x.obj[$x.attr]
    JA.AccessWithBrackets (JA.AccessWithDot "attr" ja_x) (JA.AccessWithDot "obj" ja_x)


nativeDefinitions : String
nativeDefinitions =
    """#!/usr/bin/env node
const sp_clone = (src) => {
 if (Array.isArray(src))
   return src.map(sp_clone);

 if (typeof(src) === 'object') {
   const dest = {};
   for (let k in src) { dest[k] = sp_clone(src[k]); }
   return dest;
 }

 return src;
}


//
// Basic ops
//


const sp_equal = (a) => (b) => {
  if (a === b)
    return true

  if (Array.isArray(a)) {
    if (!Array.isArray(b)) return false;

    const l = a.length;
    if (l !== b.length) return false;

    let i = 0;
    while (i < l) {
      if (!sp_equal(a[i])(b[i])) return false;
      ++i;
    }

    return true;
  }

  if (typeof(a) === 'object') {
    if (typeof(b) !== 'object') return false;

    const keys = Object.keys(a);
    const l = keys.length;
    if (l !== Object.keys(b).length) return false;

    let i = 0;
    while (i < l) {
      let k = keys[i];
      if (!sp_equal(a[k])(b[k])) return false;
      ++i;
    }

    return true;
  }

  return false;
}


const sp_not_equal = (a) => (b) => {
  return !sp_equal(a)(b);
}


const sp_compare = (a, b) => {

  // union type
  if (Array.isArray(a)) {
    // compare constructor names
    if (a[0] > b[0]) return 1;
    if (b[0] > a[0]) return -1;
    for (let i = 1; i < a.length; i++) {
        const cmp = sp_compare(a[i], b[i]);
        if (cmp) return cmp;
    }
    return 0;
  }

  if (typeof a === 'object') {
    const keys = Object.keys(a).sort();
    for (let k of keys) {
        const cmp = sp_compare(a[k], b[k]);
        if (cmp) return cmp;
    }
    return 0;
  }

  if (a > b) return 1;
  if (a < b) return -1;
  return 0;
}

const sp_divide = (right) => (left) => {
  if (right === 0) return 0;
  return left / right;
}


const basics_modBy = (a) => (b) => b % a;

const basics_compare = (a) => (b) => sp_compare(a, b);


//
// Debug
//


const sp_todo = (message) => {
  throw new Error("TODO: " + message);
}


const sp_log = (message) => (thing) => {
  console.log(message, sp_toHuman(thing));
  return thing;
}


const sp_throw = function (errorName) {
    console.error(...arguments);
    throw new Error(errorName);
}


//
// To Human
//


const sp_toHuman = (a) => {

  if (Array.isArray(a))
    return sp_toHumanAsList([], a) || sp_toHumanAsUnion(a);

  if (typeof a === 'function') {
    return '<function>';
  }

  if (typeof a === 'object') {
    let x = [];
    for (let i in a) x.push(i + ' = ' + sp_toHuman(a[i]));
    return '{' + x.join(', ') + '}';
  }

  return JSON.stringify(a, null, 0);
}


const sp_toHumanAsUnion = (a) => {
  return a[0] + ' ' + a.slice(1).map(arg => '(' + sp_toHuman(arg) + ')').join(' ');
}


const sp_toHumanAsList = (arrayAccum, list) => {
  if (list[0] === '""" ++ Core.listCons.name ++ """') {
    arrayAccum.push(sp_toHuman(list[1]));
    return sp_toHumanAsList(arrayAccum, list[2]);
  }

  if (list[0] === '""" ++ Core.listNil.name ++ """')
    return '[' + arrayAccum.join(', ') + ']';

  return false;
}


//
// Text
//


const text_fromNumber = (n) => '' + n;

const text_toNumber = (t) => {
    const n = +t;

    // TODO this is super brittle, replace it with a reference to the actual constructor names.
    // Also, we need only the names, there is no point in specifying the whole path because they just need to be unique within the union type.
    // The type checker already ensures that they will never be compared to anything else.
    return isNaN(n) ? [ "SPCore/Maybe.Nothing" ] : [ "SPCore/Maybe.Just", n ];
}

const text_split = (separator) => (target) => array_toList(target.split(separator));

const text_length = (s) => s.length;

const text_slice = (start) => (end) => (s) => s.slice(start, end);

const text_startsWith = (sub) => (s) => s.startsWith(sub);

const text_startsWithRegex = (regex) => {
  let re;
  try {
    re = new RegExp('^' + regex);
  } catch (e) {
    return () => ""
  }

  return (s) => {
    let m = s.match(re);
    return m ? m[0] : "";
  }
}

const text_replaceRegex = (regex) => {
  let re;
  try {
    re = new RegExp(regex, 'g');
  } catch (e) {
    return () => () => ""
  }

  return (replacer) => (s) => s.replace(re, replacer);
}

const text_trimLeft = (s) => {
  return s.trimLeft();
}

const text_dropLeft = (n) => (s) => {
  return s.slice(n);
}

const text_forEach = (s) => (f) => {
  for (let i of s) f(i);
  return null;
}




//
// Lists and Arrays
//


const sp_cons = (list) => (item) => {
  return [ '""" ++ Core.listCons.name ++ """', item, list];
}


const array_toList = (array) => {
  let length = array.length;
  let list = [ '""" ++ Core.listNil.name ++ """' ];
  for (let i = length - 1; i >= 0; i--) {
      list = [ '""" ++ Core.listCons.name ++ """', array[i], list ];
  }
  return list;
}


const list_toArray = (list) => {
  const array = [];
  const rec = (ls) => {
    if (ls[0] === '""" ++ Core.listNil.name ++ """')
      return array;

    array.push(ls[1]);
    return rec(ls[2]);
  };

  return rec(list);
}


const list_sortBy = (f) => (list) => array_toList(list_toArray(list).sort((a, b) => sp_compare(f(a), f(b))));


//
// Platform: IO
//
const io_wrap = (f) => [ "IO.IO", f ];

const result_ok = (a) => [ "SPCore/Result.Ok", a ];
const result_err = (e) => [ "SPCore/Result.Err", e ];

const io_parallel = (iosAsList) => io_wrap((never) => {
    // as [IO a]: IO [a]

    const ios = list_toArray(iosAsList);

    // TODO actually run them in parallel!

    let arr = [];
    for (let io of ios) {
        const r = io[1](never);
        if (r[0] === "SPCore/Result.Ok")
            arr.push(r[1]);
        else
            return result_err(r[1]);
    }

    return result_ok(array_toList(arr));
});


const io_readDir = (dirPath) => io_wrap((never) => {
    // as Text: IO [Bool & Text]

    var entries;
    try {
        entries = fs.readdirSync(dirPath, { withFileTypes: true });
    } catch (e) {
        return result_err(e.message);
    }

    return result_ok(array_toList(entries.map((dirent) => ({
        first: dirent.isDirectory(),
        second: dirent.name,
    }))));
});


const io_readFile = (path) => io_wrap((never) => {
    // as Text: IO Text

    var content;
    try {
        content = fs.readFileSync(path, 'utf8');
    } catch (e) {
        return result_err(e.message);
    }

    return result_ok(content);
});


const io_writeFile = (path) => (content) => io_wrap((never) => {
    // as Text: Text: IO None

    try {
        fs.writeFileSync(path, content);
    } catch (e) {
        return result_err(e.message);
    }

    return result_ok(null);
});


const io_writeStdout = (content) => io_wrap((never) => {
    // as Text: IO None

    console.info(content);
    return result_ok(null);
});

    """


clone : JA.Expr -> JA.Expr
clone expr =
    JA.Call (JA.Var "sp_clone") [ expr ]



{---
--- Name translations
--


   We need to give JS names to
     - the variables declared within the SP code
     - the variables we generate as part of the SP->JS translation

   and we need to ensure that these names do not clash with each other
   and do not clash with JS reserved keywords.

   So when we need a new naming schema, we follow this:

   * no $ anywhere
     => it's a native JS value

   * starts and ends with a single $
     => $clone$

   * starts with a single $
     * followed by a letter
       => it corresponds to an SP path or local value
     * followed by a number
       => it's a constructor argument

     * starts with double $$
       => $$try
       => main name of an unpacked pattern

-}


translatePath : String -> JA.Name
translatePath s =
    case Dict.get s allNatives of
        Just nv ->
            nv

        Nothing ->
            s
                |> String.replace "." "$"
                |> String.replace "/" "$"
                |> (++) "$"


constructorArgumentName : Int -> JA.Name
constructorArgumentName i =
    "$" ++ String.fromInt i


tryName : JA.Name
tryName =
    "$$try"


pickMainName : CA.Pattern -> Maybe JA.Name
pickMainName pattern =
    Maybe.map (String.replace "/" "$" >> String.replace "." "$") <|
        case pattern of
            CA.PatternAny _ name ->
                Just <| "$" ++ name

            _ ->
                pattern
                    |> CA.patternNames
                    |> Dict.keys
                    |> List.head
                    |> Maybe.map ((++) "$$")



----
--- Translation
--


getValueDefName : CA.LocalValueDef -> String
getValueDefName def =
    def.pattern
        |> CA.patternNames
        |> Dict.keys
        |> List.head
        |> Maybe.withDefault "BLARGH"


getValueRefs : Set String -> Dict String CA.RootValueDef -> CA.RootValueDef -> Set String
getValueRefs path0 functionDefs def =
    let
        path =
            -- Keep track of the defs we have descended already, so we can avoid circular recursions
            Set.insert def.name path0

        fn : CA.PosMap -> a -> M (Set String) a
        fn fold ext =
            case fold of
                CA.PosMap_Expr (CA.Variable _ args) ->
                    if args.isRoot && not (Set.member args.name path) then
                        case Dict.get args.name functionDefs of
                            Nothing ->
                                -- value is non-function, let RefHierarchy sort it normally
                                do (M.update (Set.insert args.name)) <|
                                    \_ ->
                                        return ext

                            Just functionDef ->
                                -- functions don't need to be reordered, but can be used to define static values
                                -- When this happens, all their own references need to be considered.
                                do (M.update (Set.union (getValueRefs path functionDefs functionDef))) <|
                                    \_ ->
                                        return ext

                    else
                        return ext

                _ ->
                    return ext
    in
    CA.posMap_rootValueDef fn def Set.empty
        |> Tuple.second


translateAll : ErrorEnv -> CA.AllDefs -> List JA.Statement
translateAll eenv ca =
    let
        ( aliases, unions, values ) =
            CA.split ca

        cons =
            unions
                |> Dict.values
                |> List.sortBy .name
                |> List.concatMap translateUnion

        isFunctionBlock : CA.RootValueDef -> Bool
        isFunctionBlock def =
            case def.body of
                (CA.Evaluation (CA.Lambda _ _ _)) :: [] ->
                    True

                _ ->
                    False

        ( fns, nonFns ) =
            values
                |> Dict.values
                |> List.partition isFunctionBlock

        fnsDict =
            List.foldl (\d -> Dict.insert d.name d) Dict.empty fns

        reorderedNonFuns =
            case RefHierarchy.reorder .name (getValueRefs Set.empty fnsDict) nonFns of
                Ok rnf ->
                    rnf

                Err circular ->
                    Debug.todo <| "CanonicalToJs circular non-function values: " ++ Debug.toString circular

        env =
            { mutables = Set.empty
            , errorEnv = eenv
            }

        vals =
            (fns ++ reorderedNonFuns)
                |> List.concatMap (CA.rootToLocalDef >> translateValueDef env >> Tuple.first)
    in
    nativeBinopsAsFns ++ cons ++ vals


translateValueDef : Env -> CA.LocalValueDef -> ( List JA.Statement, Env )
translateValueDef env caDef =
    if caDef.body == [] then
        ( [], env )

    else
        case pickMainName caDef.pattern of
            Nothing ->
                ( [ JA.Eval (translateBodyToExpr env caDef.body) ]
                , env
                )

            Just mainName ->
                ( (caDef.body
                    |> translateBodyToExpr env
                    |> wrapMutable caDef.mutable
                    |> JA.Define mainName
                  )
                    :: patternDefinitions mainName caDef.pattern
                , if caDef.mutable then
                    { env | mutables = Set.insert mainName env.mutables }

                  else
                    env
                )


translateStatement : Env -> CA.Statement -> ( List JA.Statement, Env )
translateStatement env s =
    case s of
        CA.Definition def ->
            translateValueDef env def

        CA.Evaluation expr ->
            ( [ JA.Eval (translateExpr env expr) ]
            , env
            )


translateVar : Env -> CA.VariableArgs -> JA.Expr
translateVar env { name, attrPath } =
    let
        jname =
            translatePath name
    in
    if Set.member jname env.mutables then
        jname
            |> unwrapMutable
            |> accessAttrs attrPath
            |> clone

    else
        accessAttrs attrPath (JA.Var jname)


translateLiteral : Types.Literal.Value -> String
translateLiteral value =
    case value of
        Types.Literal.Text t ->
            quoteAndEscape t

        Types.Literal.Number n ->
            n

        Types.Literal.Char c ->
            -- TODO how will this interact with other function calls?
            "\"" ++ String.fromChar c ++ "\""


translateExpr : Env -> CA.Expression -> JA.Expr
translateExpr env expression =
    case expression of
        CA.Literal _ lit ->
            lit
                |> translateLiteral
                |> JA.Literal

        CA.Variable _ ar ->
            translateVar env ar

        CA.Lambda pos parameter body ->
            let
                ( args, extraJaStatements, localEnv ) =
                    case parameter of
                        CA.ParameterMutable _ name ->
                            let
                                jaName =
                                    "$" ++ name
                            in
                            ( [ jaName ]
                            , []
                            , { env | mutables = Set.insert jaName env.mutables }
                            )

                        CA.ParameterPattern pattern ->
                            case pickMainName pattern of
                                Nothing ->
                                    ( []
                                    , []
                                    , env
                                    )

                                Just mainName ->
                                    ( [ mainName ]
                                    , patternDefinitions mainName pattern
                                    , env
                                    )
            in
            case translateBodyToEither localEnv extraJaStatements body of
                Lib.Left expr ->
                    JA.SimpleLambda args expr

                Lib.Right block ->
                    JA.BlockLambda args block

        CA.Record _ extends attrs ->
            let
                obj =
                    attrs
                        |> Dict.map (\k -> translateExpr env)
                        |> JA.Record
            in
            case extends of
                Nothing ->
                    obj

                Just extend ->
                    JA.Call (JA.Var "Object.assign")
                        [ JA.Record Dict.empty
                        , translateVar env extend
                        , obj
                        ]

        CA.Call _ ref arg ->
            case maybeNativeBinop env ref arg of
                Just jaExpr ->
                    jaExpr

                Nothing ->
                    let
                        jsArg =
                            translateArg { nativeBinop = False } env arg
                    in
                    case maybeNativeUnop env ref jsArg of
                        Just jaExpr ->
                            jaExpr

                        Nothing ->
                            JA.Call (translateExpr env ref) [ jsArg ]

        CA.If _ ar ->
            JA.Conditional
                (translateBodyToExpr env ar.condition)
                (translateBodyToExpr env ar.true)
                (translateBodyToExpr env ar.false)

        CA.Try pos value tries ->
            {-

               (() => {
                 const $$try = #value;

                 if (#pattern1Conditions) {
                   #pattern1Assignment
                   #pattern1Block
                   return
                 }

                 if (#pattern2Conditions) {
                   #pattern2Assignment
                   #pattern2Block
                   return
                 }
               )


            -}
            let
                head =
                    JA.Define tryName (translateExpr env value)

                init =
                    JA.Var tryName

                testPa ( pattern, block ) =
                    let
                        extraStats =
                            assignPattern pattern (JA.Var tryName) []

                        condition =
                            testPattern pattern init []
                                |> binopChain (JA.Literal "true") "&&"

                        whenConditionMatches =
                            case translateBodyToEither env extraStats block of
                                Lib.Left e ->
                                    extraStats ++ [ JA.Return e ]

                                Lib.Right bl ->
                                    -- TODO two returns is better than zero, but one would be better than two...
                                    bl ++ [ JA.Return (JA.Literal "null") ]
                    in
                    JA.If condition whenConditionMatches

                human =
                    Types.Error.posToHuman env.errorEnv pos

                default =
                    [ JA.Literal "'Missing pattern in try..as'"
                    , JA.Literal ("'" ++ human.location ++ "'")
                    , JA.Call (JA.Literal "sp_toHuman") [ JA.Var tryName ]
                    --, JA.Call (JA.Literal "console.log") [ JA.Call (JA.Literal "JSON.stringify") [ JA.Var tryName ]]
                    ]
                        |> JA.Call (JA.Literal "sp_throw")
                        |> JA.Eval

                allStatements =
                    (head :: List.map testPa tries) ++ [ default ]
            in
            JA.Call (JA.BlockLambda [] allStatements) []


translateArg : { nativeBinop : Bool } -> Env -> CA.Argument -> JA.Expr
translateArg { nativeBinop } env arg =
    case arg of
        CA.ArgumentExpression e ->
            translateExpr env e

        CA.ArgumentMutable _ { name, attrPath } ->
            if nativeBinop then
                --SP: @x.a.b += blah
                --JS: x.obj[x.attr].a.b += blah
                name
                    |> translatePath
                    |> unwrapMutable
                    |> accessAttrs attrPath

            else
                case attrPath of
                    [] ->
                        --SP: doStuff @x
                        --JS: doStuff(x)
                        name
                            |> translatePath
                            |> JA.Var

                    head :: tail ->
                        --SP: doStuff @x.a.b.c
                        --JS: doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })
                        name
                            |> translatePath
                            |> unwrapMutable
                            |> accessAttrsButTheLast head tail
                            |> (\( wrappedExpr, lastAttrName ) ->
                                    Dict.empty
                                        |> Dict.insert "obj" wrappedExpr
                                        |> Dict.insert "attr" (JA.Literal <| quoteAndEscape lastAttrName)
                                        |> JA.Record
                               )


accessAttrs : List String -> JA.Expr -> JA.Expr
accessAttrs attrPath e =
    List.foldl JA.AccessWithDot e attrPath


accessAttrsButTheLast : String -> List String -> JA.Expr -> ( JA.Expr, String )
accessAttrsButTheLast attrHead attrTail e =
    let
        fold attr ( expr, last ) =
            ( JA.AccessWithDot last expr
            , attr
            )
    in
    List.foldl fold ( e, attrHead ) attrTail


maybeNativeUnop : Env -> CA.Expression -> JA.Expr -> Maybe JA.Expr
maybeNativeUnop env reference argument =
    case reference of
        CA.Variable _ { name } ->
            case Dict.get name nativeUnops of
                Nothing ->
                    Nothing

                Just { jsSymb } ->
                    JA.Unop jsSymb argument |> Just

        _ ->
            Nothing


maybeNativeBinop : Env -> CA.Expression -> CA.Argument -> Maybe JA.Expr
maybeNativeBinop env reference argument =
    case reference of
        CA.Call _ (CA.Variable _ { name }) rightArg ->
            case Dict.get name nativeBinops of
                Nothing ->
                    Nothing

                Just { jsSymb, mutates } ->
                    let
                        cons =
                            if mutates then
                                JA.Mutop jsSymb none

                            else
                                JA.Binop jsSymb
                    in
                    cons
                        (translateArg { nativeBinop = True } env argument)
                        (translateArg { nativeBinop = True } env rightArg)
                        |> Just

        _ ->
            Nothing


binopChain : JA.Expr -> String -> List JA.Expr -> JA.Expr
binopChain default op es =
    case es of
        [] ->
            default

        head :: tail ->
            List.foldl (JA.Binop op) head tail


translateBodyToExpr : Env -> List CA.Statement -> JA.Expr
translateBodyToExpr env caBody =
    case translateBodyToEither env [] caBody of
        Lib.Left e ->
            e

        Lib.Right block ->
            JA.Call (JA.BlockLambda [] block) []


translateBodyToEither : Env -> List JA.Statement -> List CA.Statement -> Lib.Either JA.Expr (List JA.Statement)
translateBodyToEither env extra caBody =
    let
        fold caStat ( faStats, e ) =
            translateStatement e caStat
                |> Tuple.mapFirst (List.foldl (::) faStats)

        ( reversedStats, env1 ) =
            List.foldl fold ( extra, env ) caBody
    in
    case reversedStats of
        [] ->
            JA.Var "null"
                |> Lib.Left

        [ JA.Eval single ] ->
            single
                |> Lib.Left

        oldLast :: rest ->
            (case oldLast of
                JA.Eval expr ->
                    JA.Return expr :: rest

                _ ->
                    JA.Return (JA.Var none) :: oldLast :: rest
            )
                |> List.reverse
                |> Lib.Right



----
---
--


quoteAndEscape : String -> String
quoteAndEscape s =
    let
        escaped =
            s
                |> String.replace "\n" "\\n"
                |> String.replace "\"" "\\\""
    in
    "\"" ++ escaped ++ "\""


accessWithBracketsInt : Int -> JA.Expr -> JA.Expr
accessWithBracketsInt index =
    index
        |> String.fromInt
        |> JA.Var
        |> JA.AccessWithBrackets



----
--- Pattern break down
--


testPattern : CA.Pattern -> JA.Expr -> List JA.Expr -> List JA.Expr
testPattern pattern valueToTest accum =
    case pattern of
        CA.PatternDiscard _ ->
            accum

        CA.PatternAny _ name ->
            accum

        CA.PatternLiteral _ lit ->
            JA.Binop "===" (JA.Literal <| translateLiteral lit) valueToTest :: accum

        CA.PatternConstructor _ path pas ->
            let
                head =
                    -- this is necessary because we use true, false and null for True, False and None respectively
                    case Dict.get path allNatives of
                        Just nv ->
                            JA.Binop "===" (JA.Literal nv) (valueToTest)

                        Nothing ->
                            JA.Binop "===" (JA.Literal (quoteAndEscape path)) (accessWithBracketsInt 0 valueToTest)

                foldArg argPattern ( index, acc ) =
                    ( index + 1
                    , testPattern argPattern (accessWithBracketsInt index valueToTest) acc
                    )
            in
            List.foldl foldArg ( 1, head :: accum ) pas
                |> Tuple.second

        CA.PatternRecord _ attrs ->
            let
                foldAttr name pa =
                    testPattern pa (JA.AccessWithDot name valueToTest)
            in
            Dict.foldl foldAttr accum attrs


assignPattern : CA.Pattern -> JA.Expr -> List JA.Statement -> List JA.Statement
assignPattern pattern exprAccum accum =
    case pattern of
        CA.PatternDiscard _ ->
            accum

        CA.PatternAny _ name ->
            if name == "_" then
                accum

            else
                JA.Define (translatePath name) exprAccum :: accum

        CA.PatternLiteral _ literal ->
            accum

        CA.PatternConstructor _ path pas ->
            let
                foldEveryArgument ( index, pa ) =
                    assignPattern pa (accessConstructorArg (index + 1) exprAccum)
            in
            List.foldl foldEveryArgument accum (List.indexedMap Tuple.pair pas)

        CA.PatternRecord _ attrs ->
            let
                foldEveryAttr name pa =
                    assignPattern pa (JA.AccessWithDot name exprAccum)
            in
            Dict.foldl foldEveryAttr accum attrs


patternDefinitions : JA.Name -> CA.Pattern -> List JA.Statement
patternDefinitions mainName pattern =
    case pattern of
        CA.PatternAny pos _ ->
            []

        _ ->
            assignPattern pattern (JA.Var mainName) []



----
--- Union Constructors
--


accessConstructorArg : Int -> JA.Expr -> JA.Expr
accessConstructorArg =
    accessWithBracketsInt


translateUnionConstructor : ( String, List CA.Type ) -> JA.Statement
translateUnionConstructor ( consName, consArgs ) =
    -- const ConstructorName = ($1) => ($2) => [ "ConstructorName", $1, $2 ]
    let
        n =
            List.length consArgs

        range =
            List.range 1 n

        storedArgs =
            List.map (constructorArgumentName >> JA.Var) range

        name =
            quoteAndEscape consName

        expr =
            JA.Array (JA.Literal name :: storedArgs)

        lambdas =
            List.foldr (\i -> JA.SimpleLambda [ constructorArgumentName i ]) expr range
    in
    JA.Define (translatePath consName) lambdas


translateUnion : CA.UnionDef -> List JA.Statement
translateUnion def =
    def.constructors
        |> Dict.toList
        |> List.filter (\( name, args ) -> not <| Dict.member name allNatives)
        |> List.map translateUnionConstructor



--
