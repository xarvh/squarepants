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

* Source dir modules expose everything by default (except values without annotation)

* Library modules expose things by documenting them



Hoped Advantages
================
1. don't have to write "import Dict exposing (Dict)" on every single module. In fact, I don't have to write import s at all, and this is really good because I want rapid prototyping
2. you can load two versions of the same library and give it different names
3. globals are tightily controlled and there is a single place where you can see them immediately
4. No guessing what module "Cart" comes from, when you see "Cart" it's always the same module across the whole project.
5. You can have your editor load the meta file and highlight all globals/imported symbols



JSON?
====
Looks like shit.

I'd rather include a compiler command that outputs the module file in json format.

```
[
    {
        "type": "library",
        "source": "http://www.github.com/xarvh/spcore#master",
        "modules": [
            {
                "original": "Basics",
                "importAs": "Basics",
                "globals": [
                    "Bool",
                    "True",
                    "False"
                ]
            },
            {
                "original": "Dict",
                "importAs": "Dict",
                "globals": [
                    "Dict"
                ]
            }
        ]
    },
    {
        "type": "library",
        "#": "local directories can be loaded as libraries",
        "source": "lib/fast-dict/",
        "modules": [
            {
                "original": "Dict",
                "importAs": "FastDict",
                "globals": []
            }
        ]
    },
    {
        "type": "source dir",
        "#0": "Unlike libraries, all modules in the path will be made available by default,",
        "#1": "imported with their original name (determined by their file path relative",
        "#2": "to `path` below) and with no exposed globals.",
        "path": "src/",
        "#3": "Exceptions can be defined inside the [[module]] blocks below",
        "module exceptions": [
            {
                "original": "CanonicalAst",
                "importAs": "CA",
                "globals": [
                    "Expr"
                ]
            }
        ]
    }
]
```

