Stuff that is important but I can get away without for now
----------------------------------------------------------

* NonFunction errors are useless to a human; NonFunction needs more tests

* `alias A = { attr : c }` should not be valid
  -> Restore FindUndeclared

* Update syntax?

* Test number tokenization

* Add JS tests for >>, << and their functional notation

* find circular definitions / order definitions / (and dependencies?)
    * get all canonical stuff
    * start from entry, collect and order only needed stuff
    * only then run type inference
