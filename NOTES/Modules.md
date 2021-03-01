Concept
=======

All imports are specified in a single file, used by the whole project, so you
don't have to repeat them for each module.


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
```toml

[library]
  source = "http://www.github.com/xarvh/spcore#master"

  [[module]]
    original = "Basics"
    importAs = "Basics"
    globalTypes = [ "Bool" ]
    globalValues = [ "True", "False" ]

  [[module]]
    original = "Dict"
    importAs = "Dict"
    globalTypes = [ "Dict" ]


[library]
  # local directories can be loaded as libraries
  source = "lib/fast-dict/"

  [[module]]
    original = "Dict"
    importAs = "FastDict"
    globals = []


[source dir]

  # Unlike libraries, all modules in the path will be made available by default,
  # imported with their original name (determined by their file path relative
  # to `path` below) and with no exposed globals.
  path = "src/"

  # Exceptions can be defined inside the [[module]] blocks below
  [[module]]
    original = "CanonicalAst"
    importAs = "CA"
    globalTypes = [ "Expr" ]

```

* All `importAs` and `exposing` entries must be unique.

* alias names probably should not contain `.`?

* Source directories make available all modules they contain (they are loaded on demand)

* No value or type is exposed automatically from source directories.

* Libraries can be local directories or remote packages or whatever.
    The modules they expose must be listed in the config file.

* Unlike source directories, libraries define their own global modules and variables.

* Source dir modules expose everything by default (except values without annotation?)

* Library modules expose things by documenting them

* `source`, `path`, `original` can use `$(ENV_VAR)` to interpolate environment variables.
  (The compiler will fail if any referenced variable is not defined in the system env)


Hoped Advantages
================
1. don't have to write "import Dict exposing (Dict)" on every single module. In fact, I don't have to write import s at all, and this is really good because I want rapid prototyping

2. you can load two versions of the same library and give it different names

3. globals are tightily controlled and there is a single place where you can see them immediately

4. No guessing what module "Cart" comes from, when you see "Cart" it's always the same module across the whole project.

5. You can have your editor load the meta file and highlight all globals/imported symbols

6. You can expose `log` from `Debug` and then remove it enitrely or remap it to something else entirely (or just use the rename tool)

7. `Debug.toString` (or however we call it) can print the name paths that actually work in the program.



Problems
========

1. Still don't know which format to use. See below.

1. Is it ok to allow a constructor to be global but not its type?

1. Is it ok to allow a type to have some global constructors and some non-global ones?

1. Implementing the libraries system will be a pain



JSON?
====
It's a pain for humans to read and write and does not support comments.

I'd rather include a compiler command that outputs the module file in json format.

```json
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

