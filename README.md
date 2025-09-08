# hint [![Hackage](https://img.shields.io/hackage/v/hint.svg)](https://hackage.haskell.org/package/hint) [![Build Status](https://github.com/haskell-hint/hint/workflows/CI/badge.svg)](https://github.com/haskell-hint/hint/actions)

This library defines an Interpreter monad within which you can interpret
strings like `"[1,2] ++ [3]"` into values like `[1,2,3]`. You can easily
exchange data between your compiled program and your interpreted program, as
long as the data has a `Typeable` instance.

You can choose which modules should be in scope while evaluating these
expressions, you can browse the contents of those modules, and you can ask for
the type of the identifiers you're browsing.

## Example

```haskell
{-# LANGUAGE LambdaCase, ScopedTypeVariables, TypeApplications #-}
import Control.Exception (throwIO)
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer (execWriterT, tell)
import Data.Foldable (for_)
import Data.List (isPrefixOf)
import Data.Typeable (Typeable)
import qualified Language.Haskell.Interpreter as Hint

-- |
-- Interpret expressions into values:
--
-- >>> eval @[Int] "[1,2] ++ [3]"
-- Right [1,2,3]
--
-- Send values from your compiled program to your interpreted program by
-- interpreting a function:
--
-- >>> Right f <- eval @(Int -> [Int]) "\\x -> [1..x]"
-- >>> f 5
-- [1,2,3,4,5]
eval :: forall t. Typeable t
     => String -> IO (Either Hint.InterpreterError t)
eval s = Hint.runInterpreter $ do
  Hint.setImports ["Prelude"]
  Hint.interpret s (Hint.as :: t)

-- |
-- >>> :{
-- do Right contents <- browse "Prelude"
--    for_ contents $ \(identifier, tp) -> do
--      when ("put" `isPrefixOf` identifier) $ do
--        putStrLn $ identifier ++ " :: " ++ tp
-- :}
-- putChar :: Char -> IO ()
-- putStr :: String -> IO ()
-- putStrLn :: String -> IO ()
browse :: Hint.ModuleName -> IO (Either Hint.InterpreterError [(String, String)])
browse moduleName = Hint.runInterpreter $ do
  Hint.setImports ["Prelude", "Data.Typeable", moduleName]
  exports <- Hint.getModuleExports moduleName
  execWriterT $ do
    for_ exports $ \case
      Hint.Fun identifier -> do
        tp <- lift $ Hint.typeOf identifier
        tell [(identifier, tp)]
      _ -> pure ()  -- skip datatypes and typeclasses
```

Check [example.hs](examples/example.hs) for a longer example (it must be run
from hint's base directory).

## Limitations

Importing a module from the current package is not supported. It might look
like it works on one day and then segfault the next day. You have been warned.

To work around this limitation, move those modules to a separate package. Now
the part of your code which calls hint and the code interpreted by hint can
both import that module.

It is not possible to exchange a value [whose type involves an implicit kind
parameter](https://github.com/haskell-hint/hint/issues/159#issuecomment-1575629607).
This includes type-level lists. To work around this limitation, [define a
newtype wrapper which wraps the type you
want](https://github.com/haskell-hint/hint/issues/159#issuecomment-1575640606).

It is possible to run the interpreter inside a thread, but on GHC 8.8 and
below, you can't run two instances of the interpreter simultaneously.

GHC must be installed on the system on which the compiled executable is
running. There is a workaround for this but [it's not trivial](https://github.com/haskell-hint/hint/issues/80#issuecomment-963109968).

The packages used by the interpreted code must be installed in a package
database, and hint needs to be told about that package database at runtime.

The most common use case for package databases is for the interpreted code to
have access to the same packages as the compiled code (but not compiled code
itself). The easiest way to accomplish this is via a
[GHC environment file](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/packages.html#package-environments),
and the easiest way to generate a GHC environment file is
[via cabal](https://cabal.readthedocs.io/en/3.4/cabal-project.html#cfg-field-write-ghc-environment-files).
Compile your code using `cabal build --write-ghc-environment-files=always`;
this will create a file named `.ghc.environment.<something>` in the current
directory. At runtime, hint will look for that file in the current directory.

For more advanced use cases, you can use
[`unsafeRunInterpreterWithArgs`](https://hackage.haskell.org/package/hint/docs/Language-Haskell-Interpreter-Unsafe.html#v:unsafeRunInterpreterWithArgs)
to pass arguments to the underlying ghc library, such as
[`-package-db`](https://downloads.haskell.org/~ghc/latest/docs/users_guide/packages.html?highlight=package%20db#ghc-flag--package-db%20%E2%9F%A8file%E2%9F%A9)
to specify a path to a package database, or
[`-package-env`](https://downloads.haskell.org/~ghc/latest/docs/users_guide/packages.html?highlight=package%20db#ghc-flag--package-env%20%E2%9F%A8file%E2%9F%A9%7C%E2%9F%A8name%E2%9F%A9)
to specify a path to a GHC environment file.

