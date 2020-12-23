module Compiler.MutCheck exposing (..)

import Types.CanonicalAst as CA



altro casino:








{-
========================================================================
========================================================================
-}




- type inference scope/env attributes:
  * "must be mutable"

- canonical ast: variable -> "is mutable" flag



Main TODO: aggiungere mutabilita' a type inference





come funziona `@`? flag "mutable"?





v @: Int
v = 0


f : @Int -> None
f @a =
  a += 3




v = 0

f a =
  @a += 3

f v

# v e' 




alias SomeWeirdRecord =
  { a : Int
  , b : Int
  , f : Int -> Int
  }



statList x @swr y =
  @v = 0

  @swr.f =
    fn int =
      @v += 1
      Int + 2







