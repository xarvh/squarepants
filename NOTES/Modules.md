Concept
=======

All imports are specified in a single file, used by the whole project, so you
don't have to repeat them for each module.

Libraries define their own modules file and can hide modules and definitions.


Rules
=====

* No exposing values that have no annotation

* No mutable globals

* No side effects on module load


Requirements
============

* Given a module, it should be straightforward to figure out which file declares it

* Make two different versions of the same package coexist


Tooling
=======

The compiler executable can also safely perform renames all over the source dirs for:
  - module aliases
  - module names and references
  - types and symbols?


Project-wide import
===================

There is no import/use/require/include statement.

Every module available can be directly referenced from every other module in the project.

Modules are loaded only if and when referenced.

Available modules are determined by the project's single configuration file, `modules.sp`, and can come from source dirs or libraries:
```sp

library =
    source = "http://www.github.com/xarvh/spcore#master"

    module =
        original = SPCore/Maybe
        importAs = Maybe
        globalTypes = [ Maybe ]
        globalValues = [ Just, Nothing ]

    module =
        original = SPCore/Basics
        # importAs, globalTypes and globalValues can be omitted
        globalTypes = [ Bool ]
        globalValues = [ True, False, identity ]

    module =
        original = SPCore/Dict
        importAs = Dict
        globalTypes = [ Dict ]


library =
    # local directories can be loaded as libraries
    source = "lib/fast-dict/"

    module =
        # importAs allows libraries with clashing names (or even different versions of the same library) to coexist
        original = Dict
        importAs = FastDict
        globals = []


sourceDir =
    # Unlike libraries, all modules in the path will be made available by default,
    # imported with their original name (determined by their file path relative
    # to `path` below) and with no exposed globals.
    path = "src/"

    # Exceptions can be defined inside the [[module]] blocks below
    module =
        original = Types/CanonicalAst
        importAs = CA
        globalTypes = [ Expr ]


# `path` and `source` (and `original`?) fields accept system environment variables.
# This allows to use different modules for different compile targets.
# If an env variable is not defined, it will cause a compiler error.

sourceDir =
    path = "$LOCALE_CODE/translations/"
    module =
        original = L10n

library =
    source = "$MOCK_OR_PROD"
    module =
        original = HTTP
```

* All `importAs` entries must be unique.

* Source directories make available all modules they contain (they are loaded on demand)

* Source dir modules expose everything by default (except values without annotation?)

* Unlike source directories, libraries have their own modules.so and define their own global modules and variables.

* Libraries can be local directories or remote packages or whatever.
  The modules they expose must be listed in their modules.sp (format to be defined).


Hoped Advantages
================
1. don't have to write "import Dict exposing (Dict)" on every single module. In fact, I don't have to write import s at all, and this is really good because I want rapid prototyping

2. you can load two versions of the same library and give it different names

3. globals are tightily controlled and there is a single place where you can see them immediately

4. No guessing what module "Cart" comes from, when you see "Cart" it's always the same module across the whole project.

5. You can have your editor load the meta file and highlight all globals/imported symbols

6. You can expose `log` from `Debug` and then remove it enitrely or remap it to something else entirely (or just use the rename tool)

7. `toHuman` can print the module paths in the same format that the user sees and writes them, and that can be cut and pasted in the program.


Problems
========

* Is it ok to allow a constructor to be global but not its type?

* Is it ok to allow a type to have some global constructors and some non-global ones?

* Implementing the libraries system will be a pain

* How much of a problem are circular dependencies?

