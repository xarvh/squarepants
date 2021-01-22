Square Pants
============
A language for humans and other bottom dwellers

* Functional language with local mutabilty
* Type inferred
* Designed for accessibility, readability and writability



Main goals
----------

* Ergonomics and accessibility
  - easy to read for non-experts
  - syntax easy to write and quick to modify
  - more words, less symbols, symbols used to visually stand out and always indicate the same thing

* Performance
  - transpile to GLSL without significant performance penalty
  - limited ability to mutate values in place

* Readability
  - minimalist, not too expressive
  - no "magic", explicit is better than implicit
  - no global state
  - human-friendly value names

* Fast iteration
  - User code CAN use escape hatches

* Reliablity
  - No exceptions, no nulls, no undefined
  - Libraries can't be published if they use escape hatches

* Make escape hatches viable but just slightly annoying to use

* Transpile to GLSL, JavaScript/Wasm, Rust

* Can be used for scripting, compiled at run-time and executed in a safe environment



More or less established features
---------------------------------

* `(expr).attr` is not supported nor necessary (expr |> fn.attr), but it would be a nice-to-have?
  - tokenization is a mess
  - makes `@var.attr` mut values more complicated


* Different backends or "platforms" which are environments dedicated to a specific kind of apps.
  * SystemTools: file IO, server
  * Games: generic Bulk Storage / State storage functions, SFX, GFX etc, input devices...

* Allow docs for single union type constructors


* Functions
  * Evaluating an expression that produces a function without returning it is an error


* Operators
    * Algebraic binops are overloaded per GLSL, no other ops can be overloaded and no other overloading can be defined.
    * Assignment ops are not binops and are not valid expressions
    * `=` is for declaring a new symbol
    ? not-equal: `=/=`
    * use `and`, `or` instead of `&&` `||`


* imperative blocks
  ```
  someValue =

    doSomething
    + this must belong to the thing above because it starts with a binop

    this must continue on the next line because it ends with a binop +
    doSomething

    more importantly this is a do notation <| fn argument =
    function definition goes here

    function calls instead will continue on the next line only if they are
      indented
  ```

* --> data pretty printer <--
  Output debug info that is actually readable!!!

* Support `0 < x < 10`


* do notation
  Nice explanation of monad: https://stackoverflow.com/questions/44965/what-is-a-monad

  bind: "chains two operations so that they look like a single one"
  bind: "take a computation, pretend you have its result already, and use that result to produce a new computation"

  - A computation is anything that results in a value

  ```
  ch = Result.chain
  blah blah blah |> ch fn blahOutput =
  someotherline |> ch fn otherThingy =
  doStuffWith blahOutput otherThingy
  ```



### Unit type

The Preamble should contain a `type None = None`

Unit type is used as
    * Dummy parameter for functions `fn None =`
    * Return value for side effects `modifyStrings : @String -> @String -> None`?

  `()` is confusing
  `{}` is more elegant but probably just as confusing



### Pattern Matching
    * try value as pattern then ... else ...

    * try value as
          pattern then
              ...
          pattern then
              ...
          else
              ...

    Match over multiple values:

    * try v1, v2, v3 as
        p1a, p2a, p3a then
          ...
        p1b, b2b, p3b then
          ...
        else
          ...



### Support Tabs or Spaces, but not mixed
I want to wait for bootstrapping  before I implement this.

But because of accessibility tabs are important https://www.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
At the same time, also because of accessibility, spaces are important.

So, allow a file to be indented entirely with tabs or entirely with spaces, but reject a file that mixes the two




Stuff that seems good but needs thinking
----------------------------------------

* Record functional update shorthands

    # no need to repeat `oldModel`
    newModel = { oldModel with someAttribute = .someAttribute + 1 }

  This means that `.attr` can't be used as a shorthand for `fn x = x.attr`
  But maybe it can be replaced with `fn.attr`?

  Nested record updates become:

      a =
         { b = 1
         , c =
           { d = 3
           , e = 4
           }
         }

      a0 =
        { a with
        , c =
            { .c with
            , e = 4
            }
        }



* Collect-then-solve type inference:
  https://www.researchgate.net/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms
  https://github.com/cronokirby/heeren-2002/blob/master/lang.hs


* Parser error friendliness: https://www.reddit.com/r/ProgrammingLanguages/comments/k9u35g/whats_an_useful_debugging_output_for_a_simple/

* Add "naming suggestions" for container types? (Maybe, Result, List, Dict...?)

* Thorin Intermediate representation? https://compilers.cs.uni-saarland.de/papers/lkh15_cgo.pdf

* Keep currying.
  Because it's fun and makes you feel smart and fall in love with functional programming.
  It's easier to implement.
  It can also make code less readable, but I hope the advantages above will offset this.


* Remove left pipes?

* Remove point-free pipes?
    probably not, function composition is core to functional programming

* Use `>>` or `|` as value pipe?


* `risk` monop
    * `risk unionTypeValue`
    * Extract the first constructor of a Union Type, crash if not available
    * Prone to error if the constructos change order, but this is a YOLO feature





### Macros
  The only macro I really need is `TypeAST -> ExpressionAST`.

  It requires special syntax.

  Macros can throw compile errors

  Useful macros:
    order : a -> a -> Order

    toTextData : a -> TextData
    fromTextData : TextData -> Result TextDataError a

    toBinData : a -> BinData
    fromBinData : BinData -> Result BinDataError a

    clone : a -> a
    empty : a


  ```
  module Order

  ...

  macro_deriveOrderFunction : TypeAST -> ExpressionAST
  ```

  ```
  module Dict

  ...

  insert : k -> v -> Dict k v -> Dict k v
  insert key value dict =

      orderFunction = Order.macro_deriveOrderFunction k

      doStuffWith orderFunction
  ```



### function comparison/serialisation
  (A different approach would be: provide an easy way to translate (top-level?) functions to and from a union type?)


  Two functions are the same if the checksum of their canonical ast (minus position info) is the same and if their arguments are the same

  If we make a table of all the functions ever in the code, we can actually serialise and deserialize function references
  We can trim down the table to the function types that are actually used in the deserializer.

  ---> Deserializing function references is a security problem

  ---> But what about closures? Maybe we can consider those closure values as arguments (that do not affect comparison?), like I was doing with elm-glsl?




* Union Types
  * Constructors are scoped to the type: `type Blah = A | B` => `Blah.A, Blah.B`
  -> But then can we obscure them?



Game Platform
-------------

* Libraries
    LocalStorage: readable and writable
    BulkStorage: read only



GLSL
----

* Swizzling
    Allowed, as exception, for all Vector types
    (and all records where it would be unambiguous?)

* Function overloading
    Not available.
    One Vector constructor for each possibility, probably called vec4_121 or something like that.



Stuff that's still up in the air
--------------------------------



### Accessibility
https://cs.brown.edu/~sk/Publications/Papers/Published/sbk-accessible-ast-blocks/paper.pdf
https://github.com/scalacenter/advisoryboard/blob/master/proposals/016-verbal-descriptions.md



Animations with Algebraic Effects https://gopiandcode.uk/logs/log-bye-bye-monads-algebraic-effects.html
(I'm not convinced by algebraic effects, but animations are something that the language must definitely manage well)



Cool stuff from Koka https://koka-lang.github.io/koka/doc/kokaspec.html#why-perceus




Limit union type constructors to no more than two arguments?
  -> More than 2 you need to name them




Halting problem:
 https://www.reddit.com/r/ProgrammingLanguages/comments/k0hrpx/what_is_your_idea_of_a_perfect_programming/gdir42n/
 "Idris deals with this by having a class of proven-to-terminate functions. Unprovably terminating functions are still intractable, but a lot of things can be built that are provably terminating."



Modules:
  "Adding things to a module must never stop any program from compiling. That means no starred imports: every symbol is imported either explicitly, or qualified. Also no auto-importing of typeclass instances or other such implicit crap. Every little element must be imported explicitly. Modula-2 had it right,"
  https://www.reddit.com/r/ProgrammingLanguages/comments/k0hrpx/what_is_your_idea_of_a_perfect_programming/gdj455b/


No currying?
  (`List.map (blah param) list` -> `List.map (fn i = blah param i) list`)



GLSL: https://github.com/EmbarkStudios/rust-gpu


* Use * to separate type args?

* Allow `?` for skipping the annotation of a specific type?
  as in: `someAnnotatedFunction : SomeType -> ? -> SomeReturnType`


* Is it a good idea to overload ops?
  toFloat is a pain in the ass to use, but it does avoid some problems.



* Multi-line expressions:

  * Functions: Fn Arg* (BlockStart Arg (NewSiblingLine Arg)* BlockEnd)?
  ```
  funzione arg1 arg2

  funzione arg1
    arg2

  funzione
     arg1
     arg2
  ```

  * Operators:
      Term NewSiblingLine? Op NewSiblingLine? Term

      Term BlockStart Op Term BlockEnd
      Term Op BlockStart Term BlockEnd


  ```
  a + b

  a
  + b

  a +
  b

  a
  +
  b

  a
    + b

  a +
    b

  ```

  * Lambdas:
       Fn pattern+ '=' Expr
       Fn pattern+ '=' (NewSiblingLine Expr)*
       Fn pattern+ '=' BlockStart Expr (NewSiblingLine Expr)* BlockEnd
      
  ```
  fn arg = 1

  fn arg =
  1

  fn arg =
    1

  ```







* ``/``` for String and ""/""" for Text ?

* `(-) a b == b - a`?
  `(- blah)` is a function?
  `(blah -) is also a function?

* Imperative blocks can contain recursive pure expressions
  How do I handle that?
  I can be smart and divide the code in "imperative blocks" and allow hoisting/recursive definitions only across pure expressions within an "impreative block"?
  How does JavaScript deal with it?


* Currying
    doesn't solve too many problems and creates a few, so for the time being don't support it?
    it is really nice for combinators tho


* Compatible with Elm libraries?





# Statements vs Expressions
    * A function declaration consists of a list of statements
    * Statements can be:
      - Variable or function declarations
      - Variable mutations
      - function calls where the return value is ignored
      - `return` followed or not by an expression
    * Mutations and declarations DO NOT have a value and cannot be used inside expressions
    * `return` can be omitted in single-statement functions
    * `return` can be put at the end of any other statement
      - `Array.forEach (fn element = element += 1 return) anArray`


# Closures
    * Easy to implement because both JS and V have them?
    ! Pain in the ass to implement in GLSL


? System for string interpolation?

? module aliases
  - Disallow module aliases
    The first path items in module A.B.C.D can be omitted if it doesn't cause any ambiguity:
      `module Webbhuset.Control` can be referenced just as `Control` if no other module ends with `.Control`


? Do we want opaque types? Can we find a better solution?


? type alias vs alias vs type...? `type SomeAlias = SomeType`?




# for-loops
  - Use functions
  - They should be used only for performance
  - These function should stay in some module like Fast. or GLSL. ?
```

loop : state -> (state -> Bool) -> (state -> state) -> (state -> BreakOrContinue) -> Empty


loopFrom 0 (until 10) (adding 1) (fn x =
  print x
)


loopAlong (List.length list) (fn index =
  ....
)

loop 0 (fn x = x < 10) (fn x = x + 1) (fn x =
  if someCondition x then
    return Break
  else
    doStuffWith x
    return Continue




repeat 10 (fn x =
  print x
)
```



# Examples


```
rectFragmentShader : Attributes Uniforms Varying @> Maybe Color
rectFragmentShader attributes uniforms varying =

    -- TODO: transform into `pixelSize`, make it a uniform

    pixelsPerTile =
        30.0

    e =
        0.5 / pixelsPerTile

    /*
     *     0               1                            1                     0
     *     |------|--------|----------------------------|----------|----------|
     *  -edge-e  -edge  -edge+e                      edge-e      edge      edge+e
     */
    mirrorStep : Float Float @> Float
    mirrorStep edge p =
        (smoothstep (-edge - e) (-edge + e), p) - (smoothstep (edge - e) (edge + e) p)

    strokeSize =
        uniforms.dimensions / 2.0 + uniforms.strokeWidth

    fillSize =
        uniforms.dimensions / 2.0 - uniforms.strokeWidth

    alpha =
        (mirrorStep strokeSize.x localPosition.x) * (mirrorStep strokeSize.y localPosition.y)

    strokeVsFill =
        (mirrorStep fillSize.x localPosition.x) * (mirrorStep fillSize.y localPosition.y)

    color =
        mix stroke fill strokeVsFill

    return Just <| opacity * alpha * (vec4 color 1.0)
```










Obsolete
--------
  (No)

  Sarebbe utile avere le seguenti funzioni per ogni tipo:

  order : instance has order => a -> a -> Order # per dizionari/set/hashmap/sort
  toHumanReadableString : instance -> String

  toStringData : instance -> StringData
  fromStringData : (instance can decode) => StringData -> Result DecodeError instance

  toBitData : instance -> BitData
  fromBitData : (instance can decode) => BitData -> Result DecodeError instance

  Se queste funzioni vengono usate (come?) per un certo tipo, il compilatore cerca di generarle automaticamente
  Se il compilatore non riesce a generarle o l'utente vuole fare qualcosa di diverso, puo' dichiarare esplicitamente queste funzioni per quel tipo

  ```
  Dict.insert : k has order => k -> v -> Dict k v -> Dict k v
  ```

  ```
  save : someObject has toSerializedData => String -> someObject -> SideEffect (Maybe SaveError)
  save referenceName object =
    object
      |> toSerializedData
      |> writeToStorage referenceName
  ```

  ---> Is it ok if only union types can override the class functions?
  ---> Probably not! But then I have to distinguish between alias and records?


* Type inference
    Is necessary for coding fast?



  * Cannot mutate anything outside their scope
  * Declared as `$functionName : $space-separated-arguments $arrow $return type
  * Arrows:
    * `@>` functions depend entirely on their arguments and are compatible with GLSL
    * `->` functions depend entirely on their arguments
    * `#>` functions have IO effects

    * `@>` functions can be used in place of any function
    * `->` functions can be used in place of `#>` functions


  * statement blocks without arguments are evaluated at once

  * Must have at least one argument, anything without arguments is evaluated at once.
    * Maybe catch attempts at calling a function without arguments?
      ```
      x = ...

      x
      ```


Do we really need `#>` functions?
Ideally they should only be used by platform stuff.




How do I `Dict UnionType a`?
How do I `decode(SomeType) : (String or Json) -> (Maybe or Result) SomeType`

  * I need an automatic solution that works for most cases
  * But I also need to override it seamlessly
  * Further, I need to specify that a function wants a type that be decoded or used as key in a Dict

  * with typeclasses?
    --> how do I automatically generate default classes when possible?
    --> how do I avoid magic?
    --> 

  * with reflection?
    --> how do I ensure that a function can produce a certain type?
      `decode(SomeType) .... -> Blah SomeType`
      I need first-class types for this!



