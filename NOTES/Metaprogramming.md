What should it be?
==================

Macros? Templates? Something else?



Use cases
=========

* Automatically generate the `order : t -> Order` function for a given type

  This is necessary for immutable Dict and Set

  > However this could just be hardcoded


* Automatically generate encoders and decoders (text/binary)

  The user will probably want some control on how a specific type is serialized.
  What about combining automatically generated and manually written enc/decs?


* Generate GLSL?


* Automatically generate annotations for big records


* Automatically generate a union type of all the ECS components?

  Ideally I would be able to declare a component in a single file, and have it "registered" inside
  a central type automatically.
  This is a wider problem with languages like Elm, were adding a local feature requires a lot of
  changes in the parent types.

  At the same time, this feature would require metaprogramming that works across multiple files,
  possibly using an entire directory as input.

  (Albert) For example I would like to write "for each module in namespace MyThings.\* do something".

  > Such a macro would not be able to modify any of these module, just read them and generate code
  inside the module from which the macro is invoked.



How to keep it sane
===================

* Metaprogramming can't *modify* existing code, can only *add* new stuff *to the module from which is invoked*.

? (Albert) The meta programming language should be total (always terminate, not turing complete) and "type aware".

* Macros can throw compiler errors
