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
     ModuleImport(..), ModuleQualification(..), ImportList(..),
     loadModules, getLoadedModules, setTopLevelModules,
     setImports, setImportsQ, setImportsF,
     reset,
    -- ** Module querying
     ModuleElem(..), Id, name, children,
     getModuleExports,
    -- ** Annotations
    -- In the snippets below we use \'LBRACE\' and \'RBRACE\'
    -- to mean \'{\' and \'}\' respectively. We cannot put the
    -- pragmas inline in the code since GHC scarfs them up.
    getModuleAnnotations, getValAnnotations,
    -- ** Type inference
     typeChecksWithDetails,
     typeOf, typeChecks, kindOf, normalizeType,
    -- ** Evaluation
     interpret, as, infer, eval, runStmt, runStmt',
    -- * Error handling
     InterpreterError(..), GhcError(..), MultipleInstancesNotAllowed(..),
    -- * Miscellaneous
     ghcVersion, parens,
     module Control.Monad.Trans.Class,
     module Control.Monad.IO.Class,
) where

import Hint.Base
import Hint.Annotations
import Hint.InterpreterT
import Hint.Configuration
import Hint.Context
import Hint.Reflection
import Hint.Typecheck
import Hint.Eval

import Control.Monad.IO.Class
import Control.Monad.Trans.Class
