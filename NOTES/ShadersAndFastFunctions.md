
A "fast" function is a function that can be compiled to shaders.
This means that it has restrictions on what it can do.
Not sure yet about the specifics, but roughly:
* Its memory footprint must be known at compile time
* Probably we want to guarantee termination (ie, won't be Turing-complete)
* It can't call non-fast functions.



Assuming we have a piece of code that can tell use whether a function is fast or not.

* Normal `fn` functions can be either fast or slow, doesn't matter.

* `ffn` functions instead are *guaranteed* to be fast, ie the compiler will complain if they're not fast.

