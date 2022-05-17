https://www.vidarholen.net/~vidar/An_Empirical_Investigation_into_Programming_Language_Syntax.pdf


Things that should probably be implemented
------------------------------------------

# List packing instead of cons op

  use

        [ head, ...tail ]

  in place of

        head :: tail


# Partial record unpacking

  { someAttribute, ... }

  { with someAttribute }





Things that are worth considering but need thinking
---------------------------------------------------

# do-notation

---> BIG problem: what if I forget a `None ?=`?
The statement is evaluated, but silently **not inserted in the monad chain**!!!!

    # The first two are executed but ignored!!!
    checkExpression env CoreTypes.bool condition >> cb
    checkExpression env expectedType true >> cb
    checkExpression env expectedType false


This probably will be a problem regardless of the do-notation solution.
Maybe can be addressed by requiring lone evaluations to have a Debug or a mutable? Would be very ad-hoc and ugly tho.


---> Once we have [ head, ...tail ] cons, we can use `::` in place of `?`


---> The problems that the current do-notation has are:

* Does not show clearly what is happening:

* Uniform stuff does not look uniform:

      _ ?
          checkExpression env CoreTypes.bool condition >> cb

      _ ?
          checkExpression env expectedType true >> cb

      checkExpression env expectedType false


* It does not deal well with larger definition blocks:

      xxx =
          None >> dict_for attrValueByName attrName: attrValue: None:
              try Dict.get attrName attrTypeByName as
                  Nothing:
                      addCheckError pos [
                          , "This record has an attribute `" .. attrName .. "` which is not in the annotation."
                          ]

                  Just expectedAttrType:
                      checkExpression env expectedAttrType attrValue

      # The type must have all attributes that exist in the value
      None ?
          xxx >> cb


---> Possible solutions:

  Have a dedicated keyword such as `next` or `to` to put INBETWEEN statements:

  None >> dict_for attrValueByName attrName: attrValue: None:
      try Dict.get attrName attrTypeByName as
          Nothing:
              addCheckError pos [
                  , "This record has an attribute `" .. attrName .. "` which is not in the annotation."
                  ]

          Just expectedAttrType:
              checkExpression env expectedAttrType attrValue

  then do None:
  return None

  ```
  checkExpression env CoreTypes.bool condition
  on Result.ok successValue:
  checkExpression env expectedType true
  on done _:
  checkExpression env expectedType false
  ```



# Add notation to extend records

  { extend someRecord with someAttribute = 1, someOtherAttribute = 2 }

  If someRecord already has someAttribute or someOtherAttribute, the type checker should produce an error.
