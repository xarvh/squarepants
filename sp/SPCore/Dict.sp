#TODO
union Dict key value =
    Dict

empty =
    as Dict key value
    Dict

get key dict =
  as key -> Dict key value -> Maybe value
  with key NonFunction

  Nothing
