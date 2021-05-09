Stuff that is important but I can get away without for now
----------------------------------------------------------

* `alias A = { attr : c }` should not be valid
  -> Restore FindUndeclared

* Update syntax?

* Test number tokenization

* Add JS tests for >>, << and their functional notation

* find circular definitions / order definitions / (and dependencies?)
    * get all canonical stuff
    * start from entry, collect and order only needed stuff
    * only then run type inference

* CA.Ast Pos:
   alias Pos = Int
   Dict Int PosStuff?


