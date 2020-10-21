module Main exposing (..)


type OpenOrClose = Open | Close


type Token
  = Word String
  | Operator String
  | Comma
  | RoundParen OpenOrClose
  | SquareBracket OpenOrClose
  | CurlyBrace OpenOrClose
  | StringLiteral String
  | NumberLiteral String


{-| TODO should also contain info about comments and token positions?
-}
type Line = Line (List Token) (List Line)



-------




chunkToTokens c accum =
  Regex.split () c
  String.trimLeft
    |> 










type Consuming =
    Nothing
    | Word String
    | Operators String





charToStuff c =

      if isWord c then
        let
            (state, prev) =
                case consuming of
                  Word s ->
                    (state, s)
                  _ ->
                    (closePreviousToken state, "")
        in
              { state | consuming = Word c }

      else if isOperator c then
        { state | consuming = Operator c }
      else if isParen c then
        { state | tokens = Paren c :: state.tokens }
      else
        state




    Just (Word s) ->
      if isWord c then
        { state | consuming = Word <| c :: s }



      else if isOperator c then
        { state | consuming = Operator c }
      else if isParen c then
        { state | tokens = Paren c :: state.tokens }
      else
        state


    







getParent : Node -> Maybe Node
getParent n =
    Debug.todo


type alias State =
    { root : Node
    , last : Node
    }


resolveNesting : String -> State -> State
resolveNesting s state =
    case s of
        "if" ->
            case getParent state.last of

            xxx

        "then" ->
            aaaa

        "else" ->
            xxxx

        "(" ->
            xxxx

        ")" ->
            hhhh

        _ ->
          ????
