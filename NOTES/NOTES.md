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

* Documentation is a multi-line string starting with `[# DOC:`


* Module path is delimited by `/`, ie `List/Extra.find`
  (Tokens starting with uppercase letter can't be used in a division, so there is no ambiguity)


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
  to = Result.andThen
  blah blah blah >> to fn blahOutput =
  someotherline >> to fn otherThingy =
  doStuffWith blahOutput otherThingy
  ```

* imperative if, try
  Allow the branches to have different types if the block is not evaluated


### Support Tabs or Spaces, but not mixed
I want to wait for bootstrapping  before I implement this.

But because of accessibility tabs are important https://www.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
At the same time, also because of accessibility, spaces are important.

So, allow a file to be indented entirely with tabs or entirely with spaces, but reject a file that mixes the two



### Numeric types

  (+) : Num a -> Num b -> Num a b
  logN : Num F32 -> Num F32

  `Num` has a variable number of args, and means "the smallest numeric type that can contain the arguments".

  `U32` can only be unified with another `U32`
  `Num U32` can be unified with any type that can contain an `U32`

  Unsigned types: U8, U16, U32, U128
  Signed types: I8, I16, I32, I128
  Vec types: {Vec2, Vec3, Vec4}{32, 64}
  Mat types: Mat4-32, Mat4-64

  Aliases can be created for each type, then exposed as globals.

  Operations on vectors and matrices will be applied element-by-element
  => `*` is the vector dot product).
  => mat4 / vec3 means that first the vector is promoted to a mat4, then each element is divided.


  Promotion should work on both the class and the size.

  ```
  promote : NumType -> NumType -> NumType
  promote a b =
    class_c = max (class a) (class b)

    size_c = max (size a) (size b)

    # should we promote an extra to ensure that the result is contained?
    example: signed + unsigned

    makeNumType class_c size_c
  ```

Note about unsigned: https://www.reddit.com/r/ProgrammingLanguages/comments/ub5vij/what_was_considered_problematic_with_unsigned/
Unsigned might be needed for shaders or for external compatibility.
If we decide they are needed, we might make them safer by:
  * Allow conversion to and from unsigned types only via explicit call to conversion functions
  * Instead of `U*` the type names should be `Unsigned*` to discourage usage.





Stuff that seems good but needs thinking
----------------------------------------


* Shamefully copy Roc's tagged types, but without opaque constructors.

  Instead, opacity should implemented via modules.sp, so that an exposed type can be tagged as "opaque".
  This way any type alias or union or whatever can be treated, outside the library that implements it,
  as an entirely opaque type.

  --> These opaque types should probably not be extensible (but otherwise accept parameters).


* Remove the `alias` keyword entirely
  We don't declare variables as `var x = 1`, so why should we use `alias X = Int`?

      Blah x = Maybe x

  Pros: one less keyword, more concise, more elegant
  Con: less readable for noobs? The pros are very small.



https://www.reddit.com/r/ProgrammingLanguages/comments/m5sfzb/implement_a_nontrivial_hashing_algorithm_in_your/

* record & patterns

  To be entirely consistent, `{ a }` should match only `{ a : a }` and not `{ r with a : a }`

  Reading the pattern match `{ a }` one *may* intuitively think that that's all the record contains.

  If this is the case, we could have
    * `{ a }` matching only `{ a : a }`
    * `{ with a }` matching any  `{ e with a : a }`

  But is this the case?




* Collect-then-solve type inference:
  https://www.researchgate.net/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms
  https://github.com/cronokirby/heeren-2002/blob/master/lang.hs

* Algorithm W https://github.com/ebresafegaga/AlgorithmW

* Once mutation is available, switch to Algorithm J? https://github.com/jfecher/algorithm-j

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
https://www.youtube.com/watch?v=UH0A2iujtY8
https://cs.brown.edu/~sk/Publications/Papers/Published/sbk-accessible-ast-blocks/paper.pdf
https://github.com/scalacenter/advisoryboard/blob/master/proposals/016-verbal-descriptions.md


### Memory management

* vaporization memory management https://github.com/vrtbl/passerine#faq

Cool stuff from Koka https://koka-lang.github.io/koka/doc/kokaspec.html#why-perceus

https://vale.dev/blog/generational-references

https://www.reddit.com/r/ProgrammingLanguages/comments/mh1l45/garbage_collection_in_languages_with_immutable/


###

Animations with Algebraic Effects https://gopiandcode.uk/logs/log-bye-bye-monads-algebraic-effects.html
(I'm not convinced by algebraic effects, but animations are something that the language must definitely manage well)


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


* Allow `?` for skipping the annotation of a specific type?
  as in: `someAnnotatedFunction : SomeType -> ? -> SomeReturnType`


* Is it a good idea to overload ops?
  toFloat is a pain in the ass to use, but it does avoid some problems.





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





? System for string interpolation?


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

