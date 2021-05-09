Non-Root Definitions
====================

Because in SquarePants definitions can mutate variables, definition order matters.

    a @= Random.seed

    b = generateThing @a

    c = generateThing @a

# Requirement 1
To ensure that the code is understandable and deterministic, SquarePants must
guarantee that the expression for `b` is calculated before the one for `c`.

# Requirement 2
However, SquarePants must also allow for mutually recursive functions.
This means that at least to some extent, you should be able to reference functions before you define them.

# Lambda Definitions
A Lambda Definition is a definition that starts with a Lambda expression.

They are important in this context because they can never mutate a variable and they are the ones needed
for mutual recursion.


Root Definitions
================

The user should not be thinking about the order in which modules are compiled.

Because of this, SquarePants modules are not really arranged in a tree and mutual
dependencies between modules are possible, at least for now.

This requires that Non-Lambda definitions get reordered, to ensure that there are no
circular definitions and to ensure that required values are used only after they are
properly initialized.


Solutions for non-root
======================

# Functional Solution: Allow as much non-ordering as possible

-> Throw an error if a definition references a non-Lambda defined after itself.

  a = ... b

  b _ = ... c

  d = 2

  c _ = b



# Imperative solution 1: Allow just enough non-ordering as necessary for mutually recursive functions

-> Do we even need it? root mutual recursion should be enough


The general idea is that a definition can reference all lambda definitions that follow it before any non-lambda definition.

ie given:
    a = anything
    b = lambda
    c = lambda
    d = non-lambda
    e = lambda

The expression for `a` can reference `b` and `c` but not `d` and `e`.



# Imperative solution 2: Allow mutual recursion only in root

Non-root mutual recursion would be a nice-to-have, but it's not necessary.

This means that everything *must* be in dependency order, which has the advantage of being more readable.

