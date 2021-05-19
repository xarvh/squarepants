Possible alternative syntax
===========================



Things that should probably be implemented
------------------------------------------



# Distinguish `as` in `try..as` from `as` in annotations

  * try..as -> match..to
  * try..as -> try..on
  * blah x =
      is { ext with z is Int }
  * if value like



# Use `:` instead of `then` in if statements

  * if expr: blah1 else: blah2

  * if expr: blah1 else blah2

  -> Both `else` and `else:` are valid
  -> auto-formatting will turn `else` into `else:`

  + This has the advantage that `then` is not a keyword any more
  - `else:` is super ugly








Things that are worth considering but need thinking
---------------------------------------------------


# Remove support for `else` in try..as


# Use `to` instead of `->` for function types

sumTwoNumbers is Int to Int to Int
sumTwoNumbers a b =
  a + b




# Hide curryness in types

  Int, Int = Int

* `Int -> Int -> Int` is obscure.
* it would be nice to have less to type than ` -> `
* it would be nice to have a notation that's consistent with function declaration
  -> but it's not doable because of parametric polymorphism (`List Int` or `List -> Int`?)




Let's go wild
-------------



# Use `:` instead of `=`

    a_variable:
        3

    a_function: fn x y z:
        x + y + z

    alias A_Record_Type:
        { x: Int, y: Int }

    a_record_value:
        { x: 1, y: 2 }

- most languages use `=`
+ record type and value definitions are identical
+ in line with math notation that uses `:=` as defop
+ `:` is more consistently used for /defining/ stuff, rather than making statements of truth
+ `:` is more accessible than `=`
- `:` requires a Shift on an US keyboard (but `=` also does on most non-US keyboards)
+ `:` can be used without a leading space

In theory we'd have also:
+ can use `=` instead of `==` for comparison
+ removes the ambiguous use of the `=` symbol

* Use `!=`? (Which does not mean necessarily that we use unary `!`)

* But mutable numerical ops `+=` `-=` `*=` `/=` still use `=`
(Maybe I should just dump them, or use something different? Or just keep `+=` and hope no one notices?)
Also, what do we use for mutable assignment?
  Options assuming that mutation is marked:

    @x <- 4

  Options assuming that mutation is not marked:

    x @ 4
    x <- 4

  Even worse, generalize mutops:

    x +@ 4
    x *@ 4
    x ::@ 4

    @{binop} a b: a @ (a binop b)


* Should `:` be used for all block declarations?
    For example, instead of `then` in try..as:

    try blah as
      pa1:
          blah
      pa2:
          blah
      _:
          blah

      else
          blah

the one-liner looks ugly
    try blah as x: expr else expr # looks ugly

unless we go python
    try blah as x: expr else: expr

    try blah as x then expr else blah

    `if..then` would stay an exception

    if x:
      blah
    else:
      blah

    if x: blah else: blah




Obsolete
--------


# use `is` instead of `:` for HasType

    sumTwoNumbers is Int -> Int -> Int
    sumTwoNumbers a b =
      a + b

    # annotation name can be omitted
    is Int -> Int -> Int
    sumTwoNumbers a b =
      a + b

    alias Point = { x is Int, y is Int }


