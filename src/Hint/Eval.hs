module Hint.Eval (
      interpret, unsafeInterpret,
      eval, runStmt,
      parens
) where

import qualified GHC.Exts (unsafeCoerce#)

import Control.Exception

import Data.Proxy
import Data.Typeable hiding (typeOf)

import Hint.Base
import Hint.Context
import Hint.Parsers
import Hint.Util

import qualified Hint.GHC as GHC

-- | Evaluates an expression, given a witness for its monomorphic type.
interpret :: (MonadInterpreter m, Typeable a) => String -> Proxy a -> m a
interpret expr = unsafeInterpret expr . show . typeRep

unsafeInterpret :: (MonadInterpreter m) => String -> String -> m a
unsafeInterpret expr type_str =
    do -- First, make sure the expression has no syntax errors,
       -- for this is the only way we have to "intercept" this
       -- kind of errors
       failOnParseError parseExpr expr
       --
       let expr_typesig = concat [parens expr, " :: ", type_str]
       expr_val <- mayFail $ runGhc1 compileExpr expr_typesig
       --
       return (GHC.Exts.unsafeCoerce# expr_val :: a)

-- add a bogus Maybe, in order to use it with mayFail
compileExpr :: GHC.GhcMonad m => String -> m (Maybe GHC.HValue)
compileExpr = fmap Just . GHC.compileExpr

-- | @eval expr@ will evaluate @show expr@.
--  It will succeed only if @expr@ has type t and there is a 'Show'
--  instance for t.
eval :: MonadInterpreter m => String -> m String
eval expr = do in_scope_show   <- supportShow
               in_scope_String <- supportString
               let show_expr = unwords [in_scope_show, parens expr]
               unsafeInterpret show_expr in_scope_String

-- | Evaluate a statement in the 'IO' monad, possibly binding new names.
--
-- Example:
--
-- > runStmt "x <- return 42"
-- > runStmt "print x"
runStmt :: (MonadInterpreter m) => String -> m ()
runStmt = mayFail . runGhc1 go
    where
#if __GLASGOW_HASKELL__ >= 800
    go statements = do
        result <- GHC.execStmt statements GHC.execOptions
        return $ case result of
            GHC.ExecComplete { GHC.execResult = Right _ } -> Just ()
            GHC.ExecComplete { GHC.execResult = Left  e } -> throw e
            _                                             -> Nothing
#else
    go statements = do
        result <- GHC.runStmt statements GHC.RunToCompletion
        return $ case result of
            GHC.RunOk _        -> Just ()
            GHC.RunException e -> throw e
            _                  -> Nothing
#endif

-- | Conceptually, @parens s = \"(\" ++ s ++ \")\"@, where s is any valid haskell
-- expression. In practice, it is harder than this.
-- Observe that if @s@ ends with a trailing comment, then @parens s@ would
-- be a malformed expression. The straightforward solution for this is to
-- put the closing parenthesis in a different line. However, now we are
-- messing with the layout rules and we don't know where @s@ is going to
-- be used!
-- Solution: @parens s = \"(let {foo =\n\" ++ s ++ \"\\n ;} in foo)\"@ where @foo@ does not occur in @s@
parens :: String -> String
parens s = concat ["(let {", foo, " =\n", s, "\n",
                   "                     ;} in ", foo, ")"]
    where foo = safeBndFor s
