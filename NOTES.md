
Main goals
----------

* A language for indie games

* Performance
  - Must be able to transpile to GLSL without significant performance penalty
  - Must be able to mutate values in place

* Readability
  - Minimalist design
  - No "magic", explicit is better than implicit
  - No global state
  - Human-friendly symbol names

* Fast iteration
  - User code CAN use escape hatches

* Reliablity
  - No exceptions, no nulls, no undefined
  - Libraries can't be published if they use escape hatches

* Make escape hatches viable but just slightly annoying to use

* Transpile to GLSL, JavaScript, (Go | V | Rust..?)

* Can be used for scripting, compiled at run-time and executed in a safe environment



More or less established features
---------------------------------

* Different backends or "platforms" which are environments dedicated to a specific kind of apps.
  * SystemTools: file IO, server
  * Games: generic Bulk Storage / State storage functions, SFX, GFX etc, input devices...

* Allow docs for single union type constructors

* `risk` monop
    * `risk unionTypeValue`
    * Extract the first constructor of a Union Type, crash if not available
    * Prone to error if the constructos change order, but this is a YOLO feature




* Functions
  * Must have at least one argument, anything without arguments is evaluated at once.
    * Maybe catch attempts at calling a function without arguments?
      ```
      x = ...

      x
      ```
  * Cannot mutate anything outside their scope
  * Declared as `$functionName : $space-separated-arguments $arrow $return type
  * Arrows:
    * `@>` functions depend entirely on their arguments and are compatible with GLSL
    * `->` functions depend entirely on their arguments
    * `#>` functions have IO effects

    * `@>` functions can be used in place of any function
    * `->` functions can be used in place of `#>` functions

* Union Types
  * Constructors are scoped to the type: `type Blah = A | B` => `Blah.A, Blah.B`

* Modules
  - No mutable globals
  - No side effects on module load

* Operators
    * Algebraic binops are overloaded per GLSL, no other ops can be overloaded and no other overloading can be defined.
    * Assignment ops are not binops and are not valid expressions
    * `=` is for declaring a new symbol
    * `#=` is for assigning a new value to an existing mutable variable
    * not-equal: `=/=`
    ? use `and`, `or` instead of `&&` `||`

* Swizzling
    Allowed, as exception, for all Vector types

* Function overloading
    Not available.
    One Vector constructor for each possibility, probably called vec4_121 or something like that.

* Libraries
    LocalStorage: readable and writable
    BulkStorage: read only

* Anonymous Functions
    * Declared with the `fn` keyword in place of the name
    ```
    someFunction =
        fn a b c =
            a + b + c

    someMoreConvolutedFunction =
        fn a b c =
            fn z =
                a + b + c + z

    Array.mapInPlace (fn element = element + 1) anArray
    ```

* Type inference
    Is necessary for coding fast?

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

* data pretty printer

* Support `0 < x < 10`


Problems still to solve
-----------------------

What do I use as unit type?
  Unit type is used as
    * Dummy parameter for functions `fn None =`
    * Return value for side effects `writeFile : String -> String -> None`

  `()` is confusing
  `{}` is more elegant but probably just as confusing
  `type Void = Void`?
  `type None = None`?



Limit union type constructors two no more than two arguments?
  -> More than 2 you need to name them



* Pattern Matching
    * try value as pattern then ... else ...

    * try value as
          pattern then
              ...
          pattern then
              ...
          else
              ...




Halting problem:
 https://www.reddit.com/r/ProgrammingLanguages/comments/k0hrpx/what_is_your_idea_of_a_perfect_programming/gdir42n/
 "Idris deals with this by having a class of proven-to-terminate functions. Unprovably terminating functions are still intractable, but a lot of things can be built that are provably terminating."



Modules:
  "Adding things to a module must never stop any program from compiling. That means no starred imports: every symbol is imported either explicitly, or qualified. Also no auto-importing of typeclass instances or other such implicit crap. Every little element must be imported explicitly. Modula-2 had it right,"
  https://www.reddit.com/r/ProgrammingLanguages/comments/k0hrpx/what_is_your_idea_of_a_perfect_programming/gdj455b/


No currying?
  (`List.map (blah param) list` -> `List.map (fn i = blah param i) list`)



GLSL: https://github.com/EmbarkStudios/rust-gpu


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










