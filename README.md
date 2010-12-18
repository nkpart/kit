
Kit
===

A dependency management tool for Obj-C/Xcode projects.

Install
-------

    $ brew install haskell-platform
    # Make sure ~/.cabal/bin is on your PATH. 
    $ cabal install kit

Creating Kits
-------------

See [functionalkit](https://github.com/mogeneration/functionalkit) for an example of a publishable dependency. The important part of it is the KitSpec file in the root. It follows a default [structure](https://github.com/nkpart/kit/wiki/Kit-structure).

You can clone the source, and run `kit publish-local` to allow it to be depended on by other projects.

Declaring Dependencies
----------------------

