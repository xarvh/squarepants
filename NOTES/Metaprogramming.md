
TypeOf
======

Special type: `TypeOf type`: the type of the internal representation of `type`.

Operator: `typeof type`: the instance of the internal representation of `type`.

    #
    # Generate decoders
    #
    SPON.decoderFor as TypeOf a: SPON.Decoder a with a WithoutFunctions
    SPON.encoderFor as TypeOf a: a: SPON with a WithoutFunctions

    # example:
    decodeEvent as Decoder Event
        decoderFor (typeof Event)

    #
    # Dynamically compile and load code
    #
    compile as TypeOf a: CompileParams: Result Error a

    # usage:
    compile (typeof Shader) (typeof Params) params



Macros? Templates?
==================


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
