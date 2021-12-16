General Idea
============

A Platform is the interface between a pure program and the outside world.

The Platform defines the entry point of the program and its main loop, if any; this means that each application is built on top of exactly one Platform.

Everyone can write their own Platform, but, unlike libraries, not everyone can publish them.

Ideally there would be only a handful of published Platforms, each addressing a different niche use.


Interface
=========

Each platform:

* Provides a function that takes a list of definitions, in initialization order and produces... A string?

* Can override the implementation of any function/value and in fact implements all native function/values.

* Provides modules that can be used by the code (these modules are the only ones that can use Platform.native?)

* Provides a default modules.sp?


Publishing New Platforms
========================

The point of having curated Platforms is to reduce fragmentation in the ecosystem.

Because of this, popularity could very well be a good reason to publish a new platform.

This said, the standards for publishing new Platforms should be high and ideally include:

- At least one real-world application built on top of it.

- Full documentation: no one wants to waste time trying to understand how something works just to evaluate it.

- Allows to do things that were not possible before, or allow to do them significantly better!

Regardless, the whole process should be as transparent as possible.


How do we keep it sane?
=======================

The whole point of a Platform is that it can do things that the normal user code shouldn't do, possibly including side effects in synchronous code.

The risk is that instead of finding solutions within user code, the user will just go and mess with the Platform every time something is not immediately convenient, which will prevent the development of good user-space solutions.

At the same time, if the user really really wants to do something they should be able to do it.

Since this is a matter of convenience, there are a few approaches to balance this:

* Make it convenient to use existing Platforms (for example, by publishing curated ones!)

* Publish recipes and examples on how to solve particular problems with a particular Platform?

* Make it slightly annoying to mess with an existing Platform?

