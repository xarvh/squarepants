
This is a *specification* of Squarepants Mutability and Uniqueness system.
It is meant to be complete and accurate, it is **NOT** meant as newbie-friendly introduction.

Squarepants implements [uniqueness typing](http://edsko.net/2017/01/08/linearity-in-haskell/) and uses it to manage in-place mutation, track variables lifetime and manage side effects.

Uniqueness typing allows Squarepants to ensure that, at any given time, there is only a single reference to a particular item.

All the crunching is done at compile time, which means that, for unique values, the run-time does not need to keep track of allocation/destruction: no garbage collection, no reference counting needed.



How does it look like?
======================


### Example: stateful loop

    ```
    average as fn [Number]: Number =
        fn numbers:

        # Unique values can be changed in place, ie, "mutated"
        !total as Number =
            0

        !count as Number =
            0

        List.each numbers fn number:
            @total += number
            @count += 1

        # In Squarepants division by 0 yields 0
        total / count
    ```


### Example: File IO

    ```
    logToFile as fn @IO, Text: Result IO.Error None =
        fn @io, content:

        IO.openFile @io IO.Append "blah.log"
        >> isOk fn !fileDescriptor:

        IO.writeFile @io content @fileDescriptor

        # When the function ends fileDescriptor is automatically closed
    ```


Why do we want mutability via uniqueness?
=========================================

* Faster number-crunching, especially for shaders

* Some algorithms are easier to express with mutability

* Can replace several monads (State, IO, Random...)
  - Monads are trickier to understand for newcomers
  - Monads cannot be easily composed
  - Changing a piece of code to use monads often requires re-arranging a lot of code.

With uniqueness typing, we also get:

* Opt-in manual control of memory allocation

* Rust-like lifetime check of resources

* Statetypes


Design requirements and considerations
======================================

* Forbid global uniques to avoid any kind of mutable

* Prevent losing references to uniques: we should always know exactly when to deallocate one

* Mutation should visibly stand out in the code

* All things being equal, a system that it is easy to understand is better than a system that is expressive


Specification
=============


Terminology
-----------

Values in Squarepants can be either "unique" or "immutable".

Unique values must be referenced by exactly one variable at the time, but they can be "reassigned", this in practice is an in-place mutation.

Immutable values can be safely duplicated across variables and expressions, but are constant and cannot be reassigned.


Uniqueness typing
-----------------

* Every value must be either unique or immutable.


* All literal expressions (functions, number, text, constructors, records) are unique.


* Unique values can always be implicitly converted to immutable ones.
  Because of this, unique values can always be used in place of immutable ones.

  However the opposite isn't true: immutables cannot be converted to uniques.
  Using an immutable value in place of a unique one will result in a compiler error.


* Variables and arguments declared with a `!` are unique, those declared without are immutable.

    ```
    !anUniqueVariable = someUniqueExpression

    anImmutableVariable = whatever

    fn anImmutableArgument, !aUniqueArgument: ...
    ```

  A unique variable must always be initialized with a unique value.


* Referencing a unique variable *spends* it, which means that referencing the variable again will cause a compiler error.

    ```
    !x =
        1

    !y =
        # The first time we reference `x` it works!
        x

    !z =
        # But here `x` is spent, so we get a compiler error!
        x
    ```

  Likewise
    ```
    !x =
        1

    !y =
        x & x
    ```
  Will produce an error.

  This is where the "uniqueness" comes from: unique values can be passed around, but not duplicated.


* Function annotations must explicitly state the uniqueness of their arguments and return values
    ```
    aFunction as fn Int, !Int: !Int =
        fn a, !b:
        b
    ```


Reassignment and Recycling
--------------------------

* A spent variable can be reassigned:
    ```
    !x =
        0

    someFunction x

    x @=
        0
    ```

  Reassignment can happen at any time, whether the variable has been spent or not.


* Certain functions *recycle* uniques instead of spending them:

    ```
    !x =
        1

    incrementByOne @x

    # `x` now contains 2 and can be used again
    ```

  Recycling should be thought of as an implicit *spend and re-assign*, which means that
  *as far as uniqueness checking is concerned*:
    ```
    incrementByOne @x
    ```
  is equivalent to
    ```
    x @= incrementByOne x
    ```


* Recycling functions are defined by replacing the `!` in the argument with an `@`, and must guarantee
  that at the end of their execution the recycled unique is non-spent.

    ```
    aFunction as fn @Int: None =
        fn @a:

        a @= a + 1

        None
    ```

  In both the type annotation and the definition parameter, `@` flags the argument as recycled.
  The `@` automatically implies `!`.


* Calling a function that recycles a unique variable temporarily spends the variable.

  Therefore
    ```
    funz @a @a
    ```
  is invalid, because the second reference to `a` happens while the first reference has already spent it.

  (Remember that the uniqueness checker considers `funz @a @a` equivalent to `a @= funz a a`, which is invalid).


Recycling a variable in the parent scope
----------------------------------------

* A function cannot *spend* any unique variables from its closure, unless it also reassigns the variable before the function itself ends.

    ```
    !x =
        1

    f =
        # Spending `x` without reassigning it produces a compiler error
        fn y:
        x + 1

    g =
        # Spending and reassigning is fine
        fn y:
        x + 1
        x @= 0

    h =
        # Recycling is the same as spending and reassigning, so it's fine too
        fn y:
        @x += 1
    ```

  The uniqueness check behaves as if the function took an extra, implicit parameter.

    ```
    result =
        list >> List.map fn item:
            @m += 1
            item + 2

        |
        |
        v

    result & m =
        list >> List.map m fn item, m:
              item + 2 & m + 1
    ```

  This means that the function can be used only in the scope where the unique is declared,
  because otherwise it would not be possible to pass it.


* We say that a function that recycles any unique belonging to an ancestor scope (ie, any unique other than its own argument or those declared in its own closure) *requires* that unique.
    ```
    !x =
        1

    # f requires x
    f =
        fn n:
        @x += a
    ```

* An expression requires all the requirements of its sub-expressions.

* Expressions with any such requirement cannot be returned.

  --> A function call that recycles a unique also adds all its requirements to the unique.
  This is a result of the correspondence between recycling and "return and reassign":
    ```
    doStuff variableWithRequirements @x
    ```
  should be thought as
    ```
    x = doStuff variableWithRequirements x
    ```
  which means that the reassigned value of x inherits all the requirements of `variableWithRequirements`.

  --> Functions cannot add requirements to the arguments they recycle
  Again, this is a consequence of the "return and reassign" equivalence:

    ```
    addFunctions as fn @Array (fn Int: Int): None =
        fn @functions:

        @x =
            1

        f as fn Int: Int =
            fn n:
            @x += a
            n

        Array.push f @functions
    ```

  `f` requires `x`.

  `Array.push f @functions` uses `f`, and therefore it too requires `x`.

  `Array.push f @functions` should be thought as `functions = Array.push f functions`, which means that
  the reassigned `functions` value also requires `x`.

  `addFunctions` must return `functions`, but since the latter has requirements, `addFunctions` is invalid.


  --------> ISSUE: this check can get in the way when returning simple stuff like `None`
  Should probably change the rule, so that any non-function is ok?



* Expressions with requirements cannot be evaluated at a point where the unique they require has been consumed
    ```
    !x =
        1

    # f requires x
    f =
        fn n:
        @x += a

    !y =
        x

    # This will cause a compiler error, because `x` is consumed
    f 0
    ```


Polymorphism
------------

* The `!` can be replaced by an integer number followed by a `?`.
  The integer number is used as identifier for a uniqueness type variable, and indicates that the value can be used
  either as a unique, either as an immutable.

    ```
    fun as fn (fn 1?a: 2?b), 1?a: 2?b =
        fn f, 1?a:

        ...
    ```


Unions
------

* A type constructor can be given unique or immutable arguments.
  If all the arguments are unique, the constructed value will be unique, otherwise it will be immutable.

  This means that constructors are polymorphic:
  ```
      SomeConstructor as fn 1?argA, 1?argB, ...: 1?constructedValue
  ```

* When annotating a value the uniqueness modifier is not applied to the type arguments, but to the type:
  ```
  isOk as fn (fn 1?a: 2?Result error b), 1?Result error a: 2?Result error b =
      fn f, 1?a:

      ...
  ```

* Likewise, when unpacking a pattern, the uniqueness modifier is applied in front of the constructor:
  ```
  union Type = Cons Int

  !Cons v = Cons 0
  ```


Records
-------

* The record and all its attributes must have the same uniqueness.

 This means that a record literals that contains at least an immutable, will have all its other attributes forced to immutable:

  This is unique (because number literals are unique):
    ```
    { x = 0, y = 1 }
    ```

  But this is immutable (because `x` is imm, forcing `y` to imm):
    ```
    x = 0
    { x, y = 1 }
    ```

* In types and pattern unpacking, the `!` is outside of the record:
    ```
    f as fn !{ x as Int, y as Int }: Int =
        fn !{ x, y }:
        x + y
    ```

* Record access?
    ```
    # r is consumed???
    r.x
    ```

  ISSUE: this seems really useless


* The attribute of an unique record can be recycled directly:
    ```
    @record.x += 3
    ```

  This behaves as:
    ```
    !{ x, ... } = record
    @x += 3
    record @= { x, ... }
    ```

  This means that while record.x is being used, `record` is temporarily consumed.
    ```
    doStuff @record.x @record.y
    ```
  will produce an error, because `record` is temporarily consumed by the first argument.

  ---> FUTURE <---
  We can allow for the same call to unpack more than one attribute, but for now we can get away without that.



---> minor, non-blocking issue <---
An important use for records is as named arguments for function parameters.

With the system currently specced, we can't directly recycle, we need manually re-assign the uniques

    ```
    !uniqueA = ...
    !uniqueB = ...

    !record = { uniqueA, uniqueB }

    someFunction @record

    { uniqueA, uniqueB } @= aRecord
    ```

This is not ideal, but maybe not too big of a deal either?


A solution could be to syntax sugar it, ie:

    ```
    someFunction { @uniqueA, uniqueRenamed = @uniqueB }
    ```

    desugars to:

    ```
    !temp = { uniqueA, uniqueRenamed = uniqueB }
    someFunction @temp
    { uniqueA, uniqueRenamed = uniqueB } @= temp
    ```


Cloning
-------

Accessing a unique record attribute Number without consuming it is a common necessity, so cloning should be available at the very least for numbers.

    ```
    clone as fn @a: !a with a CanBeCloned =
        ...
    ```

    `CanBeCloned` works exactly like `NonFunction`, but will check if an exact type has a "non clone" flag.


Test cases
==========


PREVENT: loss of reference
--------------------------

`increment` can be executed arbitrarily, there is no way to decide when `counter` should be deallocated.

    ```
    variantA =
        !counter =
            0

        increment =
            fn None:
            @counter += 1

        increment


    variantB =
        fn @anArrayOfFunctions:

            !counter =
                0

            increment as fn None: None =
                None:
                @counter += 1

            Array.push increment @anArrayOfFunctions
    ```

Solution:
  - `increment` is tained by `counter`
  - `Array.push increment @anArrayOfFunctions` is tainted by `increment`
  - Recycling of `anArrayOfFunctions` is treated as returning its new value, but it is tainted so we cannot return it.



PREVENT: multiple spends
------------------------

    ```
    aFunction =
        fn consumeMe, somethingElse:
        ...

    aScope =
        !mutable = ...

        partiallyApplied =
            aFunction mutable

        partiallyApplied None
        partiallyApplied None
        partiallyApplied None
    ```


Solution: partial application is not a thing any more.


PREVENT: multiple spends 2
--------------------------

    ```
    d =
        !count =
            0

        List.each list item:
            count
    ```

Solution: functions cannot spend values in their closure


PREVENT: unique duplication
----------------------------

If `funz` can be called by passing the same unique twice, then its uniqueness is broken.

    ```
    !x =
        0

    funz @x @x;
    ```

Solution: the first reference to `x` temporarily consumes it, making it unavailable for the second reference.


ALLOW: mutate variable from ancestor scope
------------------------------------------

    variantA =
        !count =
            0

        List.each list fn item:
            @count += 1

    variantB =
        fn @mutable, x:
            @mutable += 1

