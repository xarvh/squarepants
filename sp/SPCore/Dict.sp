#TODO
union Dict key value =
    Dict

empty =
    is Dict key value
    Dict

get key dict =
  is key -> Dict key value -> Maybe value
  with key NonFunction

  Nothing
