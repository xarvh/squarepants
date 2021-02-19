module Compiler.CanonicalToJs exposing (..)

import Compiler.CoreModule
import Compiler.TypeInference
import Dict exposing (Dict)
import Lib
import Set exposing (Set)
import Types.CanonicalAst as CA
import Types.JavascriptAst as JA
import Types.Literal


nativeCalls : Dict CA.Name ( JA.Name, Bool )
nativeCalls =
    let
        op ca ja mutates =
            Dict.insert ca ( ja, mutates )
    in
    Dict.empty
        |> op "+" "+" False
        |> op ":=" "=" True
        |> op "+=" "+=" True


nativeValues : Dict CA.Name JA.Name
nativeValues =
    [ Tuple.pair Compiler.CoreModule.trueValue "true"
    , Tuple.pair Compiler.CoreModule.falseValue "false"
    , Tuple.pair Compiler.CoreModule.noneValue "null"
    ]
        |> Dict.fromList


none =
    translatePath Compiler.CoreModule.noneValue



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


{-| This right now just contains the names of mutable vars
-}
type alias Env =
    Set JA.Name


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


cloneDefinition : String
cloneDefinition =
    """
function sp_clone(src) {
 if (Array.isArray(src))
   return src.map(clone);

 if (typeof(src) === 'object') {
   const dest = {};
   for (let k in src) { dest[k] = sp_clone(src[k]); }
   return dest;
 }

 return src;
}
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


translatePath : CA.Path -> JA.Name
translatePath s =
    case Dict.get s nativeValues of
        Just nv ->
            nv

        Nothing ->
            "$" ++ String.replace "." "$" s


constructorArgumentName : Int -> JA.Name
constructorArgumentName i =
    "$" ++ String.fromInt i


tryName : JA.Name
tryName =
    "$$try"


pickMainName : CA.Pattern -> Maybe JA.Name
pickMainName pattern =
    case pattern of
        CA.PatternAny name ->
            Just <| "$" ++ name

        _ ->
            pattern
                |> CA.patternNames
                |> Set.toList
                |> List.head
                |> Maybe.map ((++) "$$")



----
--- Translation
--


{-| TODO add cloneDefinition
-}
translateAll : List (CA.Module e) -> List JA.Statement
translateAll mods =
    let
        unitMod mod =
            CA.extensionFold_module (\e ( a, acc ) -> ( (), acc )) ( mod, () ) |> Tuple.first
    in
    Compiler.CoreModule.coreModule
        :: List.map unitMod mods
        |> List.concatMap translateModule


{-| TODO I probably don't want to translate the whole module
-}
translateModule : CA.Module e -> List JA.Statement
translateModule ca =
    let
        cons =
            ca.unions
                |> Dict.values
                |> List.sortBy .name
                |> List.concatMap translateUnion

        env =
            Set.empty

        asDefinition s =
            case s of
                CA.Definition d ->
                    Just d

                _ ->
                    Nothing

        vals =
            List.concatMap (translateValueDef env >> Tuple.first) ca.values
    in
    cons ++ vals


translateValueDef : Env -> CA.ValueDef e -> ( List JA.Statement, Env )
translateValueDef env caDef =
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
                Set.insert mainName env

              else
                env
            )


translateStatement : Env -> CA.Statement e -> ( List JA.Statement, Env )
translateStatement env s =
    case s of
        CA.Definition def ->
            translateValueDef env def

        CA.Evaluation expr ->
            ( [ JA.Eval (translateExpr env expr) ]
            , env
            )


translateVar : Env -> CA.VariableArgs -> JA.Expr
translateVar env { path, attrPath } =
    let
        name =
            translatePath path
    in
    -- right now `env` just tells us whether a var is mutable or not
    if Set.member name env then
        name
            |> unwrapMutable
            |> accessAttrs attrPath
            |> clone

    else
        accessAttrs attrPath (JA.Var name)


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


translateExpr : Env -> CA.Expression e -> JA.Expr
translateExpr env expression =
    case expression of
        CA.Literal _ lit ->
            lit.value
                |> translateLiteral
                |> JA.Literal

        CA.Variable _ ar ->
            translateVar env ar

        CA.Lambda _ ar ->
            let
                ( args, extraJaStatements ) =
                    case pickMainName ar.parameter of
                        Nothing ->
                            ( [], [] )

                        Just mainName ->
                            ( [ mainName ]
                            , patternDefinitions mainName ar.parameter
                            )
            in
            case translateBodyToEither env extraJaStatements ar.body of
                Lib.Left expr ->
                    JA.SimpleLambda args expr

                Lib.Right block ->
                    JA.BlockLambda args block

        CA.Record _ ar ->
            let
                obj =
                    ar.attrs
                        |> Dict.map (\k -> translateExpr env)
                        |> JA.Record
            in
            case ar.maybeUpdateTarget of
                Nothing ->
                    obj

                Just extend ->
                    JA.Call (JA.Var "Object.assign")
                        [ JA.Record Dict.empty
                        , translateVar env extend
                        , obj
                        ]

        CA.Call _ ar ->
            case tryNative ar.reference of
                Just ( opConstructor, rightOperand ) ->
                    opConstructor
                        (translateArg { native = True } env ar.argument)
                        (translateArg { native = True } env rightOperand)

                Nothing ->
                    JA.Call
                        (translateExpr env ar.reference)
                        [ translateArg { native = False } env ar.argument ]

        CA.If _ ar ->
            JA.Conditional
                (translateBodyToExpr env ar.condition)
                (translateBodyToExpr env ar.true)
                (translateBodyToExpr env ar.false)

        CA.Try _ ar ->
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
                    JA.Define tryName (translateExpr env ar.value)

                init =
                    JA.Var tryName

                testPa ( pattern, block ) =
                    let
                        extraStats =
                            patternDefinitions tryName pattern

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

                allStatements =
                    head :: List.map testPa ar.patterns
            in
            JA.Call (JA.BlockLambda [] allStatements) []


translateArg : { native : Bool } -> Env -> CA.Argument e -> JA.Expr
translateArg { native } env arg =
    case arg of
        CA.ArgumentExpression e ->
            translateExpr env e

        CA.ArgumentMutable { path, attrPath } ->
            if native then
                --SP: @x.a.b += blah
                --JS: x.obj[x.attr].a.b += blah
                path
                    |> translatePath
                    |> unwrapMutable
                    |> accessAttrs attrPath

            else
                case attrPath of
                    [] ->
                        --SP: doStuff @x
                        --JS: doStuff(x)
                        path
                            |> translatePath
                            |> JA.Var

                    head :: tail ->
                        --SP: doStuff @x.a.b.c
                        --JS: doStuff({ obj: x.obj[x.attr].a.b, attr: 'c' })
                        path
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


tryNative : CA.Expression e -> Maybe ( JA.Expr -> JA.Expr -> JA.Expr, CA.Argument e )
tryNative expr =
    case expr of
        CA.Call _ ar ->
            case ar.reference of
                CA.Variable _ { path } ->
                    case Dict.get path nativeCalls of
                        Just ( op, mutates ) ->
                            Just
                                ( if mutates then
                                    JA.Mutop op none

                                  else
                                    JA.Binop op
                                , ar.argument
                                )

                        Nothing ->
                            Nothing

                _ ->
                    Nothing

        _ ->
            Nothing


binopChain : JA.Expr -> String -> List JA.Expr -> JA.Expr
binopChain default op es =
    case es of
        [] ->
            default

        head :: tail ->
            List.foldl (JA.Binop op) head tail


translateBodyToExpr : Env -> List (CA.Statement e) -> JA.Expr
translateBodyToExpr env caBody =
    case translateBodyToEither env [] caBody of
        Lib.Left e ->
            e

        Lib.Right block ->
            JA.Call (JA.BlockLambda [] block) []


translateBodyToEither : Env -> List JA.Statement -> List (CA.Statement e) -> Lib.Either JA.Expr (List JA.Statement)
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
    -- TODO escape
    "\"" ++ s ++ "\""


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
        CA.PatternDiscard ->
            accum

        CA.PatternAny name ->
            accum

        CA.PatternLiteral lit ->
            JA.Binop "===" (JA.Literal <| translateLiteral lit) valueToTest :: accum

        CA.PatternConstructor path pas ->
            let
                head =
                    JA.Binop "==="
                        (JA.Literal <| quoteAndEscape path)
                        (accessWithBracketsInt 0 valueToTest)

                foldArg argPattern ( index, acc ) =
                    ( index + 1
                    , testPattern argPattern (accessWithBracketsInt index valueToTest) acc
                    )
            in
            List.foldl foldArg ( 1, head :: accum ) pas
                |> Tuple.second

        CA.PatternRecord attrs ->
            let
                foldAttr name pa =
                    testPattern pa (JA.AccessWithDot name valueToTest)
            in
            Dict.foldl foldAttr accum attrs


assignPattern : CA.Pattern -> JA.Expr -> List JA.Statement -> List JA.Statement
assignPattern pattern exprAccum accum =
    case pattern of
        CA.PatternDiscard ->
            accum

        CA.PatternAny name ->
          if name == "_" then
            accum
          else
            JA.Define (translatePath name) exprAccum :: accum

        CA.PatternLiteral literal ->
            accum

        CA.PatternConstructor path pas ->
            let
                foldEveryArgument ( index, pa ) =
                    assignPattern pa (accessConstructorArg (index + 1) exprAccum)
            in
            List.foldl foldEveryArgument accum (List.indexedMap Tuple.pair pas)

        CA.PatternRecord attrs ->
            let
                foldEveryAttr name pa =
                    assignPattern pa (JA.AccessWithDot name exprAccum)
            in
            Dict.foldl foldEveryAttr accum attrs


patternDefinitions : JA.Name -> CA.Pattern -> List JA.Statement
patternDefinitions mainName pattern =
    case pattern of
        CA.PatternAny _ ->
            []

        _ ->
            assignPattern pattern (JA.Var mainName) []



----
--- Union Constructors
--


accessConstructorArg : Int -> JA.Expr -> JA.Expr
accessConstructorArg =
    accessWithBracketsInt


translateUnionConstructor : CA.UnionConstructor -> JA.Statement
translateUnionConstructor cons =
    -- const ConstructorName = ($1) => ($2) => [ "ConstructorName", $1, $2 ]
    let
        n =
            List.length cons.args

        range =
            List.range 1 n

        storedArgs =
            List.map (constructorArgumentName >> JA.Var) range

        name =
            quoteAndEscape cons.name

        expr =
            JA.Array (JA.Literal name :: storedArgs)

        lambdas =
            List.foldr (\i -> JA.SimpleLambda [ constructorArgumentName i ]) expr range
    in
    JA.Define (translatePath cons.name) lambdas


translateUnion : CA.UnionDef -> List JA.Statement
translateUnion def =
    def.constructors
        |> List.filter (\cons -> not <| Dict.member cons.name nativeValues)
        |> List.map translateUnionConstructor



--
