
alias Set a =
    Dict a None


empty =
  is Set a
  with a NonFunction

  Dict.empty


member a set =
  is a -> Set a -> Bool
  with a NonFunction

  Dict.get a set /= Nothing


size =
  is Set a -> Int
  with a NonFunction

  Dict.size


insert a =
  is a -> Set a -> Set a
  with a NonFunction

  Dict.insert a None


remove =
  is a -> Set a -> Set a
  with a NonFunction

  Dict.remove
