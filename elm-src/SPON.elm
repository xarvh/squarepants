module SPON exposing (..)

import Types.FormattableAst as FA
import Types.Literal as Literal


type alias Reader a =
    List FA.Statement -> Outcome a


type Outcome a
    = Accepted (List FA.Statement) a
      -- TODO add Pos
    | Rejected String
      -- TODO add Pos
    | Failed String


do : Reader a -> (a -> Reader b) -> Reader b
do readerA chainedReaderB statements =
    case readerA statements of
        Accepted newStatements a ->
            chainedReaderB a newStatements

        Rejected reason ->
            Rejected reason

        Failed reason ->
            Failed reason


return : a -> Reader a
return a statements =
    Accepted statements a


run : Reader a -> List FA.Statement -> Result String a
run readerA statements =
    case readerA statements of
        Accepted [] a ->
            Ok a

        Accepted rest a ->
            -- TODO add rest starting position
            Err "unread statements"

        Rejected r ->
            Err r

        Failed r ->
            Err r



--
-- Terms
--


string : Reader String
string statements =
    case statements of
        [ FA.Evaluation (FA.Literal pos (Literal.Text text)) ] ->
            Accepted [] text

        [ _ ] ->
            Rejected "expecting a string literal"

        _ ->
            Failed "expecting a single statement"


varName : Reader String
varName statements =
    case statements of
        (FA.Evaluation (FA.Variable pos args name)) :: tail ->
            -- TODO check args.isBinop?
            Accepted tail name

        [ _ ] ->
            Rejected "expecting a list of variable or type names"

        _ ->
            Failed "expecting a single statement"



--
-- Higher level
--


oneOf : List (Reader a) -> Reader a
oneOf readers statements =
    case readers of
        [] ->
            Rejected "options exhausted"

        headReader :: tail ->
            case headReader statements of
                Rejected _ ->
                    oneOf tail statements

                otherwise ->
                    otherwise


many : Reader a -> Reader (List a)
many readerA =
    let
        rec : List a -> Reader (List a)
        rec accum statements =
            if statements == [] then
                {-
                   Allowing emptiness at the right-hand side of `=` is ugly, but it's useful for commenting stuff out

                   globalTypes =
                       # Meh
                   globalValues = Blah

                -}
                Accepted [] (List.reverse accum)

            else
                case readerA statements of
                    Accepted tail a ->
                        rec (a :: accum) tail

                    Rejected e ->
                        Rejected e

                    Failed e ->
                        Failed e
    in
    rec []


maybe : Reader a -> Reader (Maybe a)
maybe readerA statements =
    case readerA statements of
        Accepted tail a ->
            Accepted tail (Just a)

        Rejected _ ->
            Accepted statements Nothing

        Failed r ->
            Failed r


field : String -> Reader a -> Reader a
field fieldName fieldReader statements =
    case statements of
        (FA.Definition def) :: tail ->
            case def.pattern of
                FA.PatternAny _ False name ->
                    if name == fieldName then
                        case fieldReader def.body of
                            Accepted unreadStatements a ->
                                if unreadStatements == [] then
                                    Accepted tail a

                                else
                                    Failed <| "Unread statements: " ++ Debug.toString unreadStatements

                            otherwise ->
                                otherwise

                    else
                        Rejected <| "expecting `" ++ fieldName ++ " =`"

                _ ->
                    Rejected "can't use patterns"

        _ ->
            Rejected "missing an assignment (ie `something = `)"
