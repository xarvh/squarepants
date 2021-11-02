
alias Set a =
    Dict a None


empty =
  as Set a
  with a NonFunction

  Dict.empty


member a set =
  as a: Set a: Bool
  with a NonFunction

  Dict.get a set /= Nothing


size =
  as Set a: Int
  with a NonFunction

  Dict.size


insert a =
  as a: Set a: Set a
  with a NonFunction

  Dict.insert a None


remove =
  as a: Set a: Set a
  with a NonFunction

  Dict.remove
