### 0.9.0.9

* Support GHC 9.10, GHC 9.12, and GHC 9.14
* Remove noncanonical monad instance definitions for future GHC compatibility

### 0.9.0.8

* Support GHC 9.8

### 0.9.0.7

* Support GHC 9.4
* Support GHC 9.6
* Improved documentation

### 0.9.0.6

* Fixes the 0.9.0.5 regression
* Small fix in documentation (thanks to Ed Behn)

### 0.9.0.5 (deprecated)

* Support GHC 9.2.1
* Deprecated because it breaks the common pattern of using
  'unsafeRunInterpreterWithArgs' to load a custom package database.

### 0.9.0.4

* Support GHC 9.0.1

### 0.9.0.3

* Support GHC 8.10
* Drop support for GHC 8.4
* Hint can now be used concurrently from multiple threads on GHC 8.10+

### 0.9.0.2

* Support GHC 8.8
* Drop support for GHC 8.2

### 0.9.0.1

* Make tests pass with stack 2.1.1

### 0.9.0

* Support GHC 8.6
* Drop support for GHC 8.0

### 0.8.0

* Support GHC 8.4
* Drop support for GHC 7.8 and 7.10
* Add `runStmt` to execute statements in the IO monad and bind new names
* Internal changes of temporary files for phantom modules
  - The files are now called `M<nnn>.hs` instead of `<nnn>`
  - Improved cleanup of phantom module source files
  - ghc 8.4 only: phantom modules are put into a temporary directory
* Add `typeChecksWithDetails` to obtain type-checking errors
* Stop GHC from overwriting the Ctrl-C signal handler
* Add `SetImportsF` to allow finer imports control

### 0.7.0

* Support for GHC 8.2
* Support use in a dynamically-linked executable
* Add `normalizeType`, like ghci's :kind!
* Drop support for GHC 7.6

### 0.6.0

* Support for GHC 8.0
* Add `displayException` to InterpreterError

### 0.5.2

* Add `runInterpreter` variant that takes a GHC libdir at runtime
* Add missing negated extensions to the `Extension` type
* Do not throw GHC warnings as errors

### 0.5.1

* Expose `unsafeInterpret` in `Language.Haskell.Interpreter.Unsafe`

### 0.5.0

* Drop support for GHC 7.4
* Remove deprecated functions and modules:
  - `glasgowExtensions`
  - `setUseLanguageExtensions`
  - `setInstalledModsAreInScopeQualified`
  - `Language.Haskell.Interpreter.GHC`
  - `Language.Haskell.Interpreter.GHC.Unsafe`
* Drop dependencies on `ghc-mtl` and `extensible-exceptions`

### 0.4.3

* New maintainer and source code repo

### 0.4.2.3

* It builds against 7.4.2 (not 7.4.1), so update the constraints.

### 0.4.2.2

* Builds with GHC 7.10
* Builds again with GHC 7.4
* Drops dependency on `utf8-string`

### 0.4.2.1

* Better error reporting (thanks to Corentin Dupont)

### 0.4.2.0

* Based on exceptions-0.6

### 0.4.1.0

* Based on exceptions-0.4

### 0.4.0.0

* Compiles with GHC 7.8
* Fixed an issue where `P` was available as a qualified version of
  Prelude (thanks to Samuel Gélineau)
* Uses `exceptions` package instead of `MonadCatchIO-mtl` (API breakage
  expected)
* No longer depends on `haskell-src`
* Changelog should now appear in Hackage
* Integrated unit tests with cabal

### 0.3.3.7

* Fixed a race condition that would happen, for instance, when two
  process where run one next to the other, making them, on some
  platforms, to get the same random number seed (thanks to Mario
  Pastorelli and Samuel Gélineau)
* Small fix in documentation (thanks to Daniil Frumin)

### 0.3.3.6

* Works again on GHC 7.2.x (thanks to Björn Peemöller)

### 0.3.3.5

* Works on GHC 7.4.6
* Cleans up files for phantom modules that were left behind (thanks to
  Beltram Felgenhauer)

### 0.3.3.4

* Works on GHC 7.4.1

### 0.3.3.3

* Works on GHC 7.2.1

### 0.3.3.2

* Supports GHC 7

### 0.3.3.1

* Instance declaration for Applicative (InterpreterT m) works with mtl-2
  (requires Applicative m, this shouldn't break anything...)

### 0.3.3.0

* Add unsafeRunInterpreterWithArgs
* Check that only one instance of the interpreter is run at any time

### 0.3.2.3

* Can be built against MonadCatchIO-mtl-0.3.x.x

### 0.3.2.2

* Fixed a bug that would make expressions using heavy use of the layout
  rule to fail to be interpreted (see parens)

### 0.3.2.1

* hint.cabal includes version bounds for package ghc-mtl. This is to
  avoid the accidental selection of the completely unrelated ghc-mtl
  internal to GHC and, apparently, installed in the hackage server

### 0.3.2.0

* Exports functions parens and isInterpretedModule
* Experimental support for module annotations
* Uses extensible-exceptions in order to provide a uniform interface
  across different ghc versions
* Provides an Applicative instance for IntepreterT
* Adds an option to configurate the searchPath

### 0.3.1.0

* No longer uses Language.Haskell.Extension due to configuration
  problems with Cabal. Instead, it uses its own
  Language.Haskell.Interpreter.Extension module.

### 0.3.0.0

* Updated API:
  + InterpreterT monad transformer (Interpreter = InterpreterT IO)
  + No more Sessions, use runInterpreter only once
  + New options handling functions
    - but observe that there is no setOptimizations equivalent; since
      GHC does no optimization on interpreted code, this was actually
      doing nothing
* Works with GHC 6.10 and 6.8 (untested with 6.6)

### 0.2.5

* setImportsQ added (modules can be imported both qualified and
  unqualified)

### 0.2.4.1

* BUGFIX: No longer fails on expressions ending in a -- comment

### 0.2.4

* setInstalledModsAreInScopeQualified added
* Now depends on ghc-paths (no longer needs a custom cabal script)

### 0.2.2

* setOptimizations added
* Module Language.Haskell.Interpreter.GHC.Unsafe added (contains
  unsafeSetGhcOption)
* Unit tests now based on HUnit

### 0.2.1

* BUGFIX: Module reloading was broken under 6.8
* GHC.GhcExceptions are catched and turned into InterpreterErrors

### 0.2.0.1

* Adds the requirement cabal-version < 1.3

### 0.2

* Works also with GHC 6.8 and 6.6
* Added the getModuleExports function
* withSession function throws a dynamic exception instead of returning
  Either Error a
* Requires Cabal 1.2.x
