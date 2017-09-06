-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.Interpreter
-- License     :  BSD-style
--
-- Maintainer  :  mvdan@mvdan.cc
-- Stability   :  experimental
-- Portability :  non-portable (GHC API)
--
-- A Haskell interpreter built on top of the GHC API
-----------------------------------------------------------------------------
module Language.Haskell.Interpreter(
    -- * The interpreter monad transformer
     MonadInterpreter(..), InterpreterT, Interpreter,
    -- ** Running the interpreter
     runInterpreter,
    -- ** Interpreter options
     Option, OptionVal((:=)),
     get, set,
     languageExtensions, availableExtensions, Extension(..),
     installedModulesInScope, searchPath,

    -- ** Context handling
     ModuleName, isModuleInterpreted,
     loadModules, getLoadedModules, setTopLevelModules,
     setImports, setImportsQ,
     reset, reload,
    -- ** Module querying
     ModuleElem(..), Id, name, children,
     getModuleExports,
    -- ** Annotations
    -- In the snippets below we use \'LBRACE\' and \'RBRACE\'
    -- to mean \'{\' and \'}\' respectively. We cannot put the
    -- pragmas inline in the code since GHC scarfs them up.
    getModuleAnnotations, getValAnnotations,
    -- ** Type inference
     typeOf, typeChecks, kindOf, normalizeType,
    -- ** Evaluation
     interpret, as, infer, eval, runStmt,
    -- * Error handling
     InterpreterError(..), GhcError(..), MultipleInstancesNotAllowed(..),
    -- * Miscellaneous
     ghcVersion, parens,
     module Control.Monad.Trans
) where

import Hint.Base
import Hint.Annotations
import Hint.InterpreterT
import Hint.Configuration
import Hint.Context
import Hint.Reflection
import Hint.Typecheck
import Hint.Eval

import Control.Monad.Trans
