Possible alternative syntax
---------------------------

Let's go wild.


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









# Hide curryness in types

  Int, Int: Int

* `Int -> Int -> Int` is obscure.
* it would be nice to have less to type than ` -> `
* it would be nice to have a notation that's consistent with function declaration
  -> but it's not doable because of parametric polymorphism (`List Int` or `List -> Int`?)

