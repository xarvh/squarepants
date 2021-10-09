module Main exposing (..)

import Array exposing (Array)
import Browser
import Compiler.ApplyAliases
import Compiler.ApplyAliases_Test
import Compiler.CanonicalToJs
import Compiler.CanonicalToJs_Test
import Compiler.FormattableToCanonicalAst
import Compiler.FormattableToCanonicalAst_Test
import Compiler.JsToString
import Compiler.JsToString_Test
import Compiler.Pipeline
import Compiler.ScopeCheck_Test
import Compiler.StringToTokens
import Compiler.StringToTokens_Test
import Compiler.TestHelpers
import Compiler.TokensToFormattableAst
import Compiler.TokensToFormattableAst_Test
import Compiler.TypeCheck as TC
import Compiler.TypeCheck_Test
import Css
import Dict exposing (Dict)
import GeneratedModules
import Html exposing (Html)
import Html.Attributes exposing (class, style)
import Html.Events
import Human.CanonicalAst as HumanCA
import Lib
import Markdown
import MetaFile
import OneOrMore exposing (OneOrMore)
import Parser
import Prelude
import Set exposing (Set)
import StateMonad as M exposing (M, do, return)
import Test
import Types.CanonicalAst as CA exposing (Pos)
import Types.Error exposing (ErrorEnv, Res)
import Types.FormattableAst as FA
import Types.Literal as Literal
import Types.Meta exposing (Meta)
import Types.Token as Token exposing (Token)


runTests =
    False



--         || True


moduleMain =
    ( "Main"
    , """
result = 1
      """
    )


initialFiles =
    [ [ moduleMain
      , ( metaFileName, Prelude.metaString )
--       , languageOverview
--       , moduleRandom
      ]
--     , GeneratedModules.modules
    ]
        |> List.concat
        |> List.map (Tuple.mapSecond Compiler.TestHelpers.unindent)


moduleRandom =
    ( "SPCore/Random"
    , """
[# DOC

Comments starting with `DOC` are documentation. =)

The main codebase always exposes everything.
Only libraries can hide types, values and constructors, and they are not yet supported.

#]
union Seed = Seed Number


[# DOC

This function is here just to illustrate how to use mutables.

It's very much not a practical pseudo random generator.

#]
number min max @wrappedSeed =
    as Number -> Number -> Seed @> Number

    Seed seed = wrappedSeed

    @wrappedSeed :=
      seed * 4871
          # TODO implement `modBy` =(
          # >> modBy 2147483647
          >> Seed

    # TODO implement `clamp`
    if seed > max then
        max
    else if seed < min then
        min
    else
        seed
      """
    )


metaFileName =
    "meta"


languageOverview =
    ( "Language/Overview"
    , overviewString
    )


tests =
    if not runTests then
        Html.text ""

    else
        Test.viewList
            [ Compiler.StringToTokens_Test.tests
            , Compiler.TokensToFormattableAst_Test.tests
            , Compiler.FormattableToCanonicalAst_Test.tests
            , Compiler.ScopeCheck_Test.tests
            , Compiler.ApplyAliases_Test.tests
            , Compiler.TypeCheck_Test.tests
            , Compiler.CanonicalToJs_Test.tests
            , Compiler.JsToString_Test.tests
            ]


type alias Model =
    { files : Dict String String
    , selectedFile : String
    , hidden : Set String
    }


type Msg
    = OnInput String
    | OnSelect String
    | OnToggle String


init : Model
init =
    { files = Dict.fromList initialFiles
    , hidden = Set.empty
    , selectedFile =
        --         "Language/Overview"
        initialFiles
            |> List.head
            |> Maybe.map Tuple.first
            |> Maybe.withDefault ""
    }


update : Msg -> Model -> Model
update msg model =
    case msg of
        OnInput code ->
            { model | files = Dict.insert model.selectedFile code model.files }

        OnSelect name ->
            { model | selectedFile = name }

        OnToggle id ->
            if Set.member id model.hidden then
                { model | hidden = Set.remove id model.hidden }

            else
                { model | hidden = Set.insert id model.hidden }



----
--- Helpers
--


getMeta : Model -> Result String Meta
getMeta model =
    model.files
        |> Dict.get metaFileName
        |> Maybe.withDefault ""
        |> MetaFile.stringToMeta
        --         |> Result.mapError (\e -> Types.Error.Simple { pos = -1, kind = Types.Error.Whatever e })
        |> identity


onJust : Maybe (Result e a) -> (a -> Result e b) -> Maybe (Result e b)
onJust maybeResA f =
    case maybeResA of
        Just (Ok a) ->
            f a |> Just

        _ ->
            Nothing



----
--- View
--


viewMaybeRes : ErrorEnv -> (a -> Html msg) -> Maybe (Res a) -> Html msg
viewMaybeRes eenv f maybeRes =
    case maybeRes of
        Nothing ->
            Html.text ""

        Just res ->
            case Result.mapError (Compiler.TestHelpers.errorToString eenv) res of
                Err e ->
                    Html.code [] [ Html.text e ]

                Ok a ->
                    f a


view : Model -> Html Msg
view model =
    Html.div
        [ class "col" ]
        [ viewFilesSelector model
        , Html.div
            [ class "row thirds" ]
            [ viewSelectedFile model
            , viewProgram model
            , viewTests
            ]
        , Html.node "style" [] [ Html.text Css.css ]
        ]


viewTests : Html Msg
viewTests =
    if not runTests then
        Html.div
            []
            [ Html.h6
                [ style "color" "red" ]
                [ Html.text "TESTS DISABLED" ]
            ]

    else
        Html.div
            [ class "mt ml" ]
            [ tests ]


viewFilesSelector : Model -> Html Msg
viewFilesSelector model =
    let
        viewButton name =
            Html.button
                [ Html.Events.onClick <| OnSelect name
                , Html.Attributes.disabled <| model.selectedFile == name
                , class "ml"
                ]
                [ if name == metaFileName then
                    Html.text <| name ++ ".json"

                  else
                    Html.text <| name ++ ".sp"
                ]
    in
    Html.div
        [ class "row mt"
        ]
        (model.files
            |> Dict.keys
            |> List.filter ((/=) metaFileName)
            |> List.sort
            |> (::) metaFileName
            |> List.map viewButton
        )


viewSelectedFile : Model -> Html Msg
viewSelectedFile model =
    let
        code =
            Dict.get model.selectedFile model.files |> Maybe.withDefault ""
    in
    Html.div
        [ class "col mt ml" ]
        [ viewCodeEditor model code
        , if model.selectedFile == metaFileName then
            viewMeta model code

          else
            viewFileStages model code
        ]


viewCodeEditor : Model -> String -> Html Msg
viewCodeEditor model code =
    let
        meta =
            getMeta model |> Result.withDefault Types.Meta.init

        width =
            code
                |> String.split "\n"
                |> List.map String.length
                |> List.maximum
                |> Maybe.withDefault 10
                |> max 40

        widthAttr =
            width
                |> toFloat
                |> (*) 9
                |> String.fromFloat
                |> (\s -> s ++ "px")
                |> style "width"

        height =
            code
                |> String.split "\n"
                |> List.length
                |> max 15

        heightAttr =
            height
                |> toFloat
                |> (+) 1
                |> (*) 18
                |> String.fromFloat
                |> (\s -> s ++ "px")
                |> style "height"

        viewLineNumber n =
            n
                |> String.fromInt
    in
    Html.div
        [ class "editor"
        , class "row"
        ]
        [ Html.div
            [ class "editor-line-numbers"
            , heightAttr
            ]
            [ List.range 1 height
                |> List.map String.fromInt
                |> String.join "\n"
                |> Html.text
            ]
        , Html.div
            [ class "editor-content"
            , widthAttr
            , heightAttr
            ]
            [ Html.textarea
                [ class "editor-textarea"
                , Html.Events.onInput OnInput
                , Html.Attributes.value code
                , Html.Attributes.spellcheck False
                ]
                [ Html.text code ]
            , Html.div
                [ class "editor-overlay"
                ]
                (viewSyntaxHighlight meta code)
            ]
        ]


viewSyntaxHighlight : Meta -> String -> List (Html msg)
viewSyntaxHighlight meta code =
    case Compiler.StringToTokens.lexer "<TODO module name>" code of
        Err _ ->
            [ Html.text code ]

        Ok tokens ->
            tokens
                |> List.foldl (viewColorToken meta code) ( 0, [] )
                |> Tuple.second
                |> List.reverse


viewColorToken : Meta -> String -> Token -> ( Int, List (Html msg) ) -> ( Int, List (Html msg) )
viewColorToken meta code token ( start, accum ) =
    let
        slice =
            code
                |> String.slice start token.end

        acc =
            if String.startsWith " " slice then
                Html.span [ class (tokenToClass meta token) ] [ Html.text <| String.dropLeft 1 slice ]
                    :: Html.span [] [ Html.text " " ]
                    :: accum

            else
                Html.span [ class (tokenToClass meta token) ] [ Html.text slice ] :: accum
    in
    ( token.end, acc )


tokenToClass : Meta -> Token -> String
tokenToClass meta token =
    case token.kind of
        -- Comment
        Token.Comment ->
            "comment"

        -- Terms
        Token.TextLiteral _ ->
            "literal"

        Token.NumberLiteral _ ->
            "literal"

        -- Keywords
        Token.Fn ->
            "keyword"

        Token.If ->
            "keyword"

        Token.Try ->
            "keyword"

        Token.As ->
            "keyword"

        Token.Then ->
            "keyword"

        Token.Else ->
            "keyword"

        Token.With ->
            "keyword"

        -- Ops
        Token.Defop _ ->
            "op"

        Token.Unop _ ->
            "op"

        Token.Binop _ _ ->
            "op"

        Token.Arrow _ ->
            "op"

        -- Parens
        Token.RoundParen _ ->
            "paren"

        Token.SquareBracket _ ->
            "paren"

        Token.CurlyBrace _ ->
            "paren"

        Token.Comma ->
            "paren"

        Token.Name _ "alias" ->
            "keyword"

        Token.Name _ "union" ->
            "keyword"

        Token.Name { mutable } name ->
            if mutable then
                "mutable"

            else if Dict.member name meta.globalValues || Dict.member name meta.globalTypes then
                if Compiler.FormattableToCanonicalAst.startsWithUpperChar name then
                    "globalUp"

                else
                    "globalLo"

            else if Compiler.FormattableToCanonicalAst.startsWithUpperChar name then
                "valueUp"

            else
                "valueLo"

        _ ->
            ""


viewMeta : Model -> String -> Html Msg
viewMeta model code =
    case getMeta model of
        Err s ->
            Html.text s

        Ok meta ->
            meta
                |> Debug.toString
                |> Html.text


viewFileStages : Model -> String -> Html Msg
viewFileStages model rawCode =
    let
        eenv =
            { metaFile = { sourceDirs = [], libraries = [] }
            , moduleByName = Dict.map (\k v -> { fsPath = k, content = v }) model.files
            }

        code =
            Compiler.TestHelpers.unindent rawCode

        tokens =
            onJust (Just <| Ok code) (Compiler.StringToTokens.lexer model.selectedFile)

        faModule =
            onJust tokens (Compiler.TokensToFormattableAst.parse code model.selectedFile)

        caModule =
            case getMeta model of
                Ok meta ->
                    let
                        ro =
                            { meta = meta
                            , code = code
                            , currentModule = model.selectedFile
                            }
                    in
                    onJust faModule (\fa -> Compiler.FormattableToCanonicalAst.translateModule ro fa Dict.empty)

                Err e ->
                    Just <| Types.Error.errorTodo e
    in
    Html.div
        []
        [ viewHide "Canonical AST"
            model
            [ viewMaybeRes eenv viewCanonicalAst caModule
            ]
        , viewHide "Formattable AST"
            model
            [ viewMaybeRes eenv viewFormattableAst faModule
            ]
        , viewHide "Tokens"
            model
            [ viewMaybeRes eenv viewTokens tokens
            ]
        ]


viewHide : String -> Model -> List (Html Msg) -> Html Msg
viewHide id model content =
    let
        isHidden =
            Set.member id model.hidden
    in
    Html.div
        []
        (viewHideCheckbox id model
            :: (if isHidden then
                    []

                else
                    content
               )
        )


viewHideCheckbox : String -> Model -> Html Msg
viewHideCheckbox id model =
    let
        isHidden =
            Set.member id model.hidden
    in
    Html.div
        [ class "row pointer"
        , Html.Events.onClick <| OnToggle id
        ]
        [ Html.input
            [ Html.Attributes.type_ "checkbox"
            , Html.Attributes.checked <| not isHidden
            ]
            []
        , Html.div
            []
            [ Html.text id ]
        ]


viewProgram : Model -> Html Msg
viewProgram model =
    let
        do =
            Lib.result_do

        eenv =
            { metaFile = { sourceDirs = [], libraries = [] }
            , moduleByName = Dict.map (\k v -> { fsPath = k, content = v }) model.files
            }

        compileAndInsert : Meta -> String -> String -> CA.AllDefs -> Result String CA.AllDefs
        compileAndInsert meta fileName code acc =
            if fileName == metaFileName then
                Ok acc

            else
                code
                    |> Compiler.TestHelpers.unindent
                    |> (\fa -> Compiler.Pipeline.stringToCanonicalAst meta fileName fa)
                    |> Result.map (Dict.union acc)
                    |> Result.mapError (Compiler.TestHelpers.errorToString eenv)

        emitModule : CA.AllDefs -> Result x String
        emitModule caModule =
            caModule
                |> Compiler.CanonicalToJs.translateAll
                |> List.map (Compiler.JsToString.emitStatement 0)
                |> (++) [ Compiler.CanonicalToJs.nativeDefinitions ]
                |> String.join "\n\n"
                |> Ok

        programResult meta =
            do (Lib.dict_foldRes (compileAndInsert meta) model.files Prelude.prelude) <| \allDefs ->
            let
                withAliases : Result String CA.AllDefs
                withAliases =
                    allDefs
                        |> Compiler.ApplyAliases.applyAliasesToModule
                        |> Result.mapError (Compiler.TestHelpers.errorToString eenv)
            in
            do withAliases <| \alsDefs ->
            alsDefs
                |> TC.allDefsToEnvAndValues
                |> (\( env, values ) -> TC.fromAllValueDefs env values)
                |> Result.mapError (Compiler.TestHelpers.errorToString eenv)

        titleAndPreCode title text =
            Html.li
                []
                [ Html.h6 [] [ Html.text title ]
                , Html.pre [] [ Html.code [] [ Html.text text ] ]
                ]
    in
    Html.ul
        [ class "ml mt border"
        , style "padding-right" "1em"
        ]
        (case getMeta model of
            Err e ->
                [ Html.text e ]

            Ok meta ->
                case programResult meta of
                    Err e ->
                        [ Html.pre
                            []
                            [ Html.text e ]
                        ]

                    Ok env ->
                        -- subs, program ) ->
                        {-
                           [ titleAndPreCode
                               "JavaScript value for Mod.result:"
                               (case Compiler.JsToString_Test.runProgram "Main.result" subs program of
                                   Ok res ->
                                       res

                                   Err message ->
                                       "Error: ### " ++ message ++ " ###"
                               )
                           , program
                               |> emitModule subs
                               |> Result.withDefault "error"
                               |> titleAndPreCode "Evaluated JavaScript code:"
                           ]
                        -}
                        [ Html.text "All ok"
                        , env.instanceVariables
                            |> Dict.filter (\name _ -> not <| Dict.member name Prelude.prelude)
                            |> Dict.toList
                            |> List.map
                                (\( name, va ) ->
                                    Html.li
                                        [ style "min-width" "400px" ]
                                        [ Html.pre
                                            []
                                            [ (name ++ " as " ++ HumanCA.typeToString va.ty ++ " ### [" ++ Debug.toString va.freeTypeVariables ++ "]: ")
                                                |> Html.text
                                            ]
                                        ]
                                )
                            |> Html.ul []
                        ]
        )



----
--- Inference
--
-- viewInference : (TC.Eas -> Html msg
{-
   viewInference ( mod, env, subs ) =
       Html.div
           []
           [ env
               |> Dict.toList
               |> List.filter (\( k, v ) -> not (Dict.member k preamble))
               |> List.map (\( k, v ) -> Html.div [] [ k ++ ": " ++ viewSchema v |> Html.text ])
               |> Html.div []

           {-
              , mod.values
                  |> Dict.values
                  --             |> List.sortBy .name
                  |> List.map viewCaDefinition
                  |> Html.code []
           -}
           , subs
               |> Dict.toList
               |> List.map (\( k, v ) -> Html.div [] [ k ++ " => " ++ viewCaType v |> Html.text ])
               |> Html.div []
           ]
-}
-- viewSchema : TC.EnvEntry -> String
-- viewSchema schema =
--     [ "forall: [" ++ String.join "," (Set.toList schema.forall) ++ "]"
--     , "mutable: " ++ Debug.toString schema.mutable
--     , "type: " ++ HumanCA.typeToString schema.type_
--     ]
--         |> String.join " ### "
{-
   ----
   --- Undeclared
   --


   viewUndeclared : Result (List Compiler.FindUndeclared.Error) Compiler.FindUndeclared.EnvUn -> Html msg
   viewUndeclared un =
       case un of
           Err errors ->
               let
                   viewError error =
                       case error of
                           Compiler.FindUndeclared.ErrorValueUsedBeforeDeclaration name locations ->
                               Html.div [ style "color" "red" ] [ Html.text <| "value `" ++ name ++ "` used before declaration at locations: " ++ Debug.toString locations ]

                           Compiler.FindUndeclared.ErrorUndeclaredTypeVariable name locations ->
                               Html.div [ style "color" "red" ] [ Html.text <| "type variable `" ++ name ++ "` used at " ++ Debug.toString locations ++ " was not declared" ]
               in
               Html.div
                   []
                   (List.map viewError errors)

           Ok m ->
               Html.div
                   []
                   [ Html.div [] [ Html.text <| "types: " ++ Debug.toString m.types ]
                   , Html.div [] [ Html.text <| "values: " ++ Debug.toString m.values ]
                   ]
-}
----
--- Canonical AST
--


viewCanonicalAst : CA.AllDefs -> Html msg
viewCanonicalAst mod =
    let
        v ( name, rv ) =
            case rv of
                CA.Alias a ->
                    viewCaAlias a

                CA.Union u ->
                    viewCaUnion u

                CA.Value d ->
                    viewCaRootDefinition d
                        |> indentToString
                        |> Html.text
    in
    Html.pre
        []
        (mod
            |> Dict.toList
            |> List.sortBy Tuple.first
            |> List.map v
        )


viewCaAlias : CA.AliasDef -> Html msg
viewCaAlias al =
    Html.pre
        []
        [ [ "alias"
          , al.name
          , String.join " " al.args
          , "="
          , HumanCA.typeToString al.ty
          ]
            |> String.join " "
            |> Html.text
        ]


viewCaUnion : CA.UnionDef -> Html msg
viewCaUnion u =
    Html.div
        []
        [ [ "union:"
          , u.name
          , String.join " " u.args
          , "="
          , Debug.toString u.constructors
          ]
            |> String.join " "
            |> Html.text
        ]



----
---


type Indent
    = S String
    | P Indent
    | M (Maybe String)
    | I Indent
    | L (List Indent)


indentToString : Indent -> String
indentToString indent =
    indentToStringRec 0 indent []
        |> List.reverse
        |> String.join "\n"


indentToStringRec : Int -> Indent -> List String -> List String
indentToStringRec cur ind acc =
    let
        shift s =
            String.repeat cur "  " ++ s
    in
    case ind of
        S s ->
            shift s :: acc

        M (Just s) ->
            shift s :: acc

        M Nothing ->
            acc

        P i ->
            case indentToStringRec cur i [] of
                [] ->
                    acc

                [ a ] ->
                    (shift "(" ++ a ++ ")") :: acc

                many ->
                    acc
                        |> (::) (shift "(")
                        |> indentToStringRec (cur + 1) i
                        |> (::) (shift ")")

        I i ->
            indentToStringRec (cur + 1) i acc

        L inds ->
            List.foldl (indentToStringRec cur) acc inds



----


viewCaLocalDefinition : CA.LocalValueDef -> Indent
viewCaLocalDefinition def =
    L
        [ def.maybeAnnotation
            |> Maybe.map (\x -> viewCaPattern def.pattern ++ " : " ++ HumanCA.typeToString x.ty)
            |> M
        , S <| viewCaPattern def.pattern ++ " = "
        , I <| L <| List.map viewCaStatement def.body
        ]


viewCaRootDefinition : CA.RootValueDef -> Indent
viewCaRootDefinition def =
    L
        [ def.maybeAnnotation
            |> Maybe.map (\x -> def.name ++ " : " ++ HumanCA.typeToString x.ty)
            |> M
        , S <| def.name ++ " = "
        , I <| L <| List.map viewCaStatement def.body
        ]


viewCaParameter : CA.Parameter -> String
viewCaParameter p =
    case p of
        CA.ParameterPattern pa ->
            viewCaPattern pa

        CA.ParameterMutable pos pa ->
            "@" ++ pa


viewCaPattern : CA.Pattern -> String
viewCaPattern p =
    case p of
        CA.PatternDiscard _ ->
            "_"

        CA.PatternAny _ n ->
            n

        CA.PatternLiteral _ l ->
            "(" ++ Debug.toString l ++ ")"

        CA.PatternConstructor _ n pas ->
            if pas == [] then
                n

            else
                "(" ++ n ++ " " ++ String.join " " (List.map viewCaPattern pas) ++ ")"

        _ ->
            Debug.toString p


viewCaStatement : CA.Statement -> Indent
viewCaStatement s =
    case s of
        CA.Evaluation expr ->
            P <| viewCaExpression expr

        CA.Definition def ->
            I <| viewCaLocalDefinition def


viewCaVariableArgs : CA.VariableArgs -> Indent
viewCaVariableArgs s =
    s.name
        :: List.map (\a -> "." ++ a) s.attrPath
        |> String.join ""
        |> S


viewCaExpression : CA.Expression -> Indent
viewCaExpression expr =
    case expr of
        CA.Literal _ (Literal.Text s) ->
            S s

        CA.Literal _ (Literal.Number s) ->
            S s

        CA.Literal _ (Literal.Char s) ->
            S <| Debug.toString s

        CA.Variable _ s ->
            viewCaVariableArgs s

        CA.Call _ reference argument ->
            L
                [ viewCaExpression reference
                , I
                    (case argument of
                        CA.ArgumentMutable _ args ->
                            S <| "@" ++ args.name ++ String.join ":" args.attrPath

                        CA.ArgumentExpression e ->
                            viewCaExpression e
                    )
                ]

        CA.Lambda _ param body ->
            P <|
                L <|
                    [ S <| "fn " ++ viewCaParameter param ++ " ="
                    , I <| L <| List.map viewCaStatement body
                    ]

        CA.Record _ extends attrs ->
            L <|
                [ S "{"
                , extends
                    |> Maybe.map (\e -> [ viewCaVariableArgs e, S " with " ])
                    |> Maybe.withDefault []
                    |> L
                , attrs
                    |> Dict.toList
                    |> List.sortBy Tuple.first
                    |> List.map (\( n, e ) -> L [ S <| n ++ " = ", viewCaExpression e ])
                    |> L
                , S "}"
                ]

        --CA.Try
        _ ->
            ()
                |> CA.posMap_expression (\_ _ -> return CA.S) expr
                |> Tuple.second
                |> Debug.toString
                |> S



----
--- Formattable AST
--


viewFormattableAst : FA.Module -> Html msg
viewFormattableAst statements =
    statements
        |> List.map viewFaStatement
        |> Html.div []


viewFaStatement : FA.Statement -> Html msg
viewFaStatement s =
    case s of
        FA.UnionDef td ->
            Html.div
                []
                [ td
                    |> Debug.toString
                    |> (++) "type definition: "
                    |> Html.text
                ]

        FA.TypeAlias td ->
            Html.div
                []
                [ td
                    |> Debug.toString
                    |> (++) "type alias: "
                    |> Html.text
                ]

        FA.Evaluation expr ->
            Html.div
                []
                [ Html.text "Evaluation: "
                , viewFaExpression expr
                ]

        FA.Definition { pattern, maybeAnnotation, body } ->
            Html.div
                []
                [ Html.span
                    []
                    [ Html.text (viewFaPattern pattern)
                    ]
                , maybeAnnotation
                    |> Maybe.map (\x -> " : " ++ Debug.toString x)
                    |> Maybe.withDefault ""
                    |> Html.text
                , body
                    |> List.map viewFaStatement
                    |> Html.div []
                ]


viewFaPattern : FA.Pattern -> String
viewFaPattern p =
    case p of
        FA.PatternAny pos mutable n ->
            n

        _ ->
            Debug.toString p


viewFaExpression : FA.Expression -> Html msg
viewFaExpression expr =
    case expr of
        FA.Literal pos s ->
            Html.text (Debug.toString s)

        FA.Variable pos { isBinop } s ->
            Html.text s

        FA.FunctionCall pos reference arguments ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text "[call] "
                    , viewFaExpression reference
                    ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    (List.map viewFaExpression arguments)
                ]

        FA.Binop pos group sepList ->
            Html.div
                [ style "border" "red" ]
                [ Html.div
                    []
                    [ Html.text <| "[op] " ]

                --TODO ++ op ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    []

                --TODO viewFaExpression left ]
                , Html.div
                    []
                    [ Html.text "---" ]
                , Html.div
                    [ style "padding-left" "2em" ]
                    []

                --TODO viewFaExpression right ]
                ]

        --     Unop String Expression
        _ ->
            Html.code
                []
                [ expr
                    |> Debug.toString
                    |> Html.text
                ]



----
--- Tokens
--


viewTokens : List Token -> Html msg
viewTokens tokens =
    tokens
        |> List.map (\t -> Html.div [] [ Html.text (Debug.toString t.kind) ])
        |> Html.div []



----
---
--


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


overviewString =
    """
[#
   SquarePants has no import statements: instead, project-wide imports are
   declared in the `meta` file.

   Among other things, this makes it really easy to flag globals, for example
   by underscoring them.
#]


# Declare a constant
numberOne =
    1

# Declare a function
addThreeNumbers x y z =
    x + y + z


# TODO: polymorphism for number types is not yet implemented,
# so I'm cheating and adding these aliases.
alias Int = Number
alias Float = Number
alias Vec2 = Number


# Declarations can have a type annotation
floatSix =
    as Float

    # parens are not needed for calling functions
    addThreeNumbers 1 2 3


fibonacci n =
    as Int -> Int

    # if-then-else always yields a value
    if n < 2 then n else n + fibonacci (n - 1)


subtractTwoFrom n =
    as Vec2 -> Vec2

    # operators can be used prefixed, like functions
    # `left - right` becomes `(-) right left`
    (-) 2 n


# Square brackets for Lists.
# All items must be of the same type
listOfText =
    as [ Text ]
    [
    , "Gary"
    , "Bikini Bottom"
    , "I'm ready! Promotion!"
    ]


# `>>` and `<<` are just syntactic sugar, read them as "send to".
# They help using less parens and help visualizing how a value is
# transformed step-by-step.
repeatHello times =
    as Int -> Text
    "Hello!"
        >> List.repeat times
        >> Text.join ", "
        >> (..) " And append this text at the end"


# When you see `@`, it means "this stuff is mutable"
average numbers =
    as List Int -> Float

    # mutable variables can only be local and can't leave their scope.
    # `average` is still a pure function.
    n @= 0
    sum @= 0

    # anonymous functions start with `fn`
    List.each numbers fn x:
        @n += 1
        @sum += x

    # division by 0 yields 0
    sum / n


# The argument preceding `@>` is mutable
generateTwoRandomNumbers min max @seed =
    as Int -> Int -> Random.Seed @> Int & Int

    # '&' is used for tuples
    Random.number min max @seed & Random.number min max @seed



# Algebraic Data Types

union LoadingState payload =
    , NotRequested
    , Requested
    , Error Text
    , Available payload

getStatusName loadingState =
    as LoadingState payload -> Text

    try loadingState as
        NotRequested: "Not needed"
        Requested: "Awaiting server response"
        Error message: "Error: " .. message
        Available _: "Successfully loaded"

getPayload loadingState =
    as LoadingState payload -> Maybe payload

    # TBH not sure if single-line try..as should be a thing
    try loadingState as Available payload: Just payload else Nothing



# Records

alias Crab =
    {
    , name as Text
    , money as Float
    }

eugeneKrabs =
    as Crab
    {
    , name = "Eugene H. Krabs"
    , money = 2 #TODO 2_345_678.90
    }


# TODO add a record access example


earnMoney profit crab =
    as Float -> Crab -> Crab

    # `.money` is a shorthand for `crab.money`
    { crab with money = .money + profit }


# to-notation
getAllHouses getAsset =
    as (Text -> Maybe house) -> Maybe { rock as house, moai as house, pineapple as house }

    to = Maybe.andThen
    getAsset "rock" >> to fn rock:
    getAsset "moai" >> to fn moai:
    getAsset "pineapple" >> to fn pineapple:
       Just { rock, moai, pineapple }
    """
        |> String.trim
