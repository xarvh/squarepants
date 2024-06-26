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


* `(expr).attr` is not supported nor necessary (expr |> fn.attr), but it would be a nice-to-have?
  - tokenization is a mess
  - makes `@var.attr` mut values more complicated


* Different backends or "platforms" which are environments dedicated to a specific kind of apps.
  * SystemTools: file IO, server
  * Games: generic Bulk Storage / State storage functions, SFX, GFX etc, input devices...

* Allow docs for single union type constructors


* Functions
  * Evaluating an expression that produces a function without returning it is an error


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


* imperative if, try
  Allow the branches to have different types if the block is not evaluated


### Support Tabs or Spaces, but not mixed
I want to wait for bootstrapping  before I implement this.

But because of accessibility tabs are important https://www.reddit.com/r/javascript/comments/c8drjo/nobody_talks_about_the_real_reason_to_use_tabs/
At the same time, also because of accessibility, spaces are important.

So, allow a file to be indented entirely with tabs or entirely with spaces, but reject a file that mixes the two



Stuff that seems good but needs thinking
----------------------------------------


https://www.reddit.com/r/ProgrammingLanguages/comments/m5sfzb/implement_a_nontrivial_hashing_algorithm_in_your/

* Collect-then-solve type inference:
  https://www.researchgate.net/publication/2528716_Generalizing_Hindley-Milner_Type_Inference_Algorithms
  https://github.com/cronokirby/heeren-2002/blob/master/lang.hs

* Algorithm W https://github.com/ebresafegaga/AlgorithmW

* Once mutation is available, switch to Algorithm J? https://github.com/jfecher/algorithm-j

* Parser error friendliness: https://www.reddit.com/r/ProgrammingLanguages/comments/k9u35g/whats_an_useful_debugging_output_for_a_simple/

* Add "naming suggestions" for container types? (Maybe, Result, List, Dict...?)

* Thorin Intermediate representation? https://compilers.cs.uni-saarland.de/papers/lkh15_cgo.pdf

* Remove left pipes?



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


GLSL: https://github.com/EmbarkStudios/rust-gpu


* Allow `?` for skipping the annotation of a specific type?
  as in: `someAnnotatedFunction : SomeType -> ? -> SomeReturnType`


* Imperative blocks can contain recursive pure expressions
  How do I handle that?
  I can be smart and divide the code in "imperative blocks" and allow hoisting/recursive definitions only across pure expressions within an "impreative block"?
  How does JavaScript deal with it?


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

