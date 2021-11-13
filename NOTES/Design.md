A language for humans and other bottom dwellers.


Language Overview
-----------------

    [#
       SquarePants has no module header or import statements: instead, project-wide imports are
       declared in `modules.sp` file.

       This keeps module aliases consistent across the app and allows a few tricks, such as
       allow different versions of the same libraries to coexist, or help your editor highlight
       global values.

       An example of modules.sp is [here](https://github.com/xarvh/squarepants/blob/make-canonical/modules.sp).
    #]


    # Declare a constant
    numberOne =
        1

    # Declare a function
    addThreeNumbers x y z =
        x + y + z


    # TODO: polymorphism for number types is not yet implemented,
    # so I'm cheating and adding these aliases.
    alias Int = Number
    alias Float = Number
    alias Vec2 = Number


    # Declarations can have a type annotation
    floatSix =
        as Float

        # parens are not needed for calling functions
        addThreeNumbers 1 2 3


    fibonacci n =
        as Int: Int

        # if-then-else always yields a value
        if n < 2: n else: n + fibonacci (n - 1)


    subtractTwoFrom n =
        as Vec2: Vec2

        # operators can be used prefixed, like functions
        # `left - right` becomes `(-) right left`
        # (this is opposite to what Haskell or Elm do)
        (-) 2 n


    # Square brackets mean "List".
    # All items must be of the same type
    listOfText =
        as [ Text ]
        [
        , "Gary"
        , "Bikini Bottom"
        , "I'm ready! Promotion!"
        ]


    # `>>` and `<<` are just syntactic sugar, read them as "send to".
    # They help using less parens and help visualizing how a value is
    # transformed step-by-step.
    repeatHello times =
        as Int: Text
        "Hello!"
            >> List.repeat times
            >> Text.join ", "
            >> (..) " And append this text at the end"


    # When you see `@`, it means "this thing is mutable"
    # If you come from Haskell, think of it as sintactic sugar around the State monad.
    average numbers =
        as List Int: Float

        # mutable variables can only be local and can't leave their scope.
        # `average` is still a pure function.
        n @= 0
        sum @= 0

        # anonymous functions start with `fn`
        List.each numbers fn x:
            @n += 1
            @sum += x

        # division by 0 yields 0
        sum / n


    # The argument preceding `@:` is mutable
    generateTwoRandomNumbers min_ max_ @seed =
        as Int: Int: Random.Seed @: Int & Int

        # '&' is used for tuples
        Random.number min_ max_ @seed & Random.number min_ max_ @seed



    # Algebraic Data Types
    # (or "union" types? "sum"? How do you call them so that non-programmers understand?)

    union LoadingState payload =
        , NotRequested
        , Requested
        , Error Text
        , Available payload

    getStatusName loadingState =
        as LoadingState payload: Text

        try loadingState as
            NotRequested: "Not needed"
            Requested: "Awaiting server response"
            Error message: "Error: " .. message
            Available _: "Successfully loaded"

    getPayload loadingState =
        as LoadingState payload: Maybe payload

        try loadingState as
            Available payload: Just payload
            _: Nothing



    # Records

    alias Crab =
        {
        , name as Text
        , money as Float
        }

    eugeneKrabs =
        as Crab
        {
        , name = "Eugene H. Krabs"
        , money = 2 #TODO 2_345_678.90
        }


    # TODO add a record access example


    earnMoney profit crab =
        as Float: Crab: Crab

        # `.money` is a shorthand for `crab.money`
        { crab with money = .money + profit }


    # pseudo do-notation
    getAllHouses getAsset =
        as (Text: Maybe house): Maybe { rock as house, moai as house, pineapple as house }

        getAsset "rock" >> Maybe.onJust fn rock:
        getAsset "moai" >> Maybe.onJust fn moai:
        getAsset "pineapple" >> Maybe.onJust fn pineapple:
       Just { rock, moai, pineapple }



Why did I start this project?
-----------------------------

I wanted to write videogames.

I did not enjoy writing code in the object-oriented languages that most game engines use.

Rust's Bevy Engine is really good, but when I tried using it I found that I was not enjoying it.
This might be because my inability to focus very well clashed with a lot of the implicit stuff the engine was doing, and the rich syntax and powerful expression of the Rust language.
I wanted something more minimalist, more accessible.

I really like the Elm language and its focus on human-centered design, but it is a poor fit for video games and I'm not fond of its community model.

I spend a lot of time codeing and thinking about How Things Could Be Better (TM), so I had a lot of ideas for a language floating about my head.

My videogame projects failed because I was not enjoying writing them and I found myself out of options, so I thought I might as well try.


Non-requirements
----------------

It's easy to say that your language is obvious to read, and fast to write, and incredibly powerful and expressive and super performant, but in the end you need to compromise.

What is Squarepants willing to sacrifice?

* Expressive power. Sometimes the language will require you to write more, or be less elegant or mathematically consistent. Simplicity and readability are more important.

* Close-to-metal performance. Squarepants values performance but is no Rust or C++, it is meant to be a higher level language.

* Doing everything: Squarepants is not the right tool for everything, it wants to be good only for games and interactive apps.


Design requirements
-------------------

Squarepants does not have a single over-arching principle.

It is an attempt to harmonize a mish-mash of disparate ideas, many incremental improvements rather than a single overall principle.

Squarepants design is a compromise, a collection of requirements and priorities that are often in contrast with each other and still need the refinement that only comes with actually trying to use it.

Let's start from the requirements from videogames:

* Squarepants should be able to crunch numbers efficiently. Doesn't have to be a Rust, but should be at least as fast as JavaScript.
    - Squarepants supports confined mutability.
    - Squarepants will use a Perceus-like memory manager to reduce the number of re-allocations.
    - Squarepants will compile to a low-level representation such as WebAssembly.
* You should be able to write graphic shaders directly in Squarepants, and they should be **as fast** as you'd written them in GLSL.
* Games written in Squarepants should be easily portable to many different platforms.
* You should be able to quickly test new ideas without having to modify half of the codebase every time you change a type (I am looking at you, Elm...)
* First-class support for matrices and vectors.

What about developer experience?

* Squarepants should talk like a human who doesn't understand too much about programming. It should be tested by people who are not necessarily competent programmers.
* Squarepants should be accessible:
    - Different people with different input and reading devices (and cultures and habits and bodies and minds) should be able to use it.
    - Not everyone is using a US keyboard.
    - Not everyone knows, or can easily type, a pipe character or backtick.
    - It's better to read and write letters than symbols.
    - Indents can be done with either spaces or tabs (but not both in the same module).
* Squarepants should aim at readability over expressive power.
    - When in doubt over a feature, don't add it.
    - Symbols, weird mathematical squiggles and infix operators should be reduced to a minimum.
* Squarepants should be explicit: you should be able to quickly verify for yourself what a piece of code actually does and what does not do.
    - No magic, no hidden side effects, no stuff happening under the hood.
    - No run-time exceptions, no monkey-patching, no typeclasses, no operator overloading.
    - No implicit casting.
* "If it compiles, it works".
    - A powerful type system allows you to make invalid states impossible.
    - It should be hard to write a program that can actually crash, and the compiler should give you strong guarantees about it.
* You should not have to write more than needed, both to write faster and to reduce clutter:
    - When readability is the same, prefer less or easier keystrokes
    - No module headers or imports
    - Use significant white space
* No side-effects other than confined mutability


Special Features
----------------

* Mutability (see NOTES/Mutability.md)
* No import statements (see NOTES/Modules.md)
* Platforms (?)


If you come from Elm
--------------------

This is a very non-exhaustive list of differences:

* No `module` declarations, no `import` statements, everything is centralized.
* Every non-function is `comparable`, which means that you can use union types as Dict keys.
* Encoders and decoders are generated automatically from types, so instead of writing a decoder you define a type.
* No need to type `let..in`. However, statements will be evaluated in order and you can't refer to values that haven't yet been defined.
* Every union type constructor or root value is always public by default. If you want to hide something, you need to define a library.
* Many ops and symbols are changed (for example, `->` is replaced with `:`)
* The precedence order is slightly changed, so that you can use lambdas with less parens, which is especially useful in the pseudo do-notation:
    ```
    thisFunctionReturnsAResultSomething arg1 arg2 >> Result.onOk fn something:
    thisIsAResultSomethinElse arg3 >> Result.onOk fn somethingElse:
    Ok << buildStuffWith something somethingElse
    ```
*   ```
    { aRecord
        | attr1 = f aRecord.attr1
        , attr2 = g aRecord.attr2
    }
    ```
    becomes
    ```
    { aRecord with
        , attr1 = f .attr1
        , attr2 = g .attr2
    }
    ```
    i.e. `.attribute` is a shorthand for record updates and can't be used as a functon (maybe I'll add a different syntax for that, but for now I want to see what happens without).
* Unused code will not even be type checked. This is meant to make it easier to experiment without having to fix errors across the whole codebase, just comment out a couple of function calls.
* `{ a = a, b = b }` can be rewritten as `{ a, b }`, which is very handy when using `Debug.log`.

