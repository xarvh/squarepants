Rules
=====

* No exposing values that have no annotation

* No mutable globals

* No side effects on module load



Requirements
============

* Given a module, it should be straightforward to figure out which file declares it

* Make two different versions of the same package coexist



Project-wide import
===================

There is no import/use/require/include statement.

Every module available can be directly referenced from every other module in the project.

TODO: what about circular dependencies?

Modules are loaded only if and when referenced.

Available modules are determined by the project's single configuration file, and can come from source dirs or libraries:
```

[library]
  source = http://www.github.com/xarvh/spcore#master

  [[module]]
    original = Basics
    importAs = Basics
    globals = [ Bool, True, False ]

  [[module]]
    original = Dict
    importAs = Dict
    globals = [ Dict ]


[library]
  # local directories can be loaded as libraries
  source = lib/fast-dict/

  [[module]]
    original = Dict
    importAs = FastDict
    globals = []


[source dir]

  # Unlike libraries, all modules in the path will be made available by default,
  # imported with their original name (determined by their file path relative
  # to `path` below) and with no exposed globals.
  path = src/

  # Exceptions can be defined inside the [[module]] blocks below
  [[module]]
    original = CanonicalAst
    importAs = CA
    globals = [ Expr ]

```

* All `importAs` and `exposing` entries must be unique.

* Source directories make available all modules they contain (they are loaded on demand)

* No value or type is exposed automatically from source directories.

* Libraries can be local directories or remote packages or whatever.
    The modules they expose must be listed in the config file.

* Unlike source directories, libraries define their own global modules and variables.

