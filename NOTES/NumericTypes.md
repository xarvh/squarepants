
This is a draft design of the special Number type.

It needs to be updated to seamlessly support different architectures, which will be a pain to properly do.

The main targets are WASM and SPIR-V, anything else is secondary.

https://webassembly.github.io/spec/core/syntax/types.html
https://registry.khronos.org/SPIR-V/specs/unified1/SPIRV.html#OpTypeInt


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

