module Hint.InterpreterT (
    InterpreterT, Interpreter,
    runInterpreter, runInterpreterWithArgs, runInterpreterWithArgsLibdir,
    MultipleInstancesNotAllowed(..)
) where

import Control.Applicative
import Prelude

import Hint.Base
import Hint.Context
import Hint.Configuration
import Hint.Extension

import Control.Monad.Reader
import Control.Monad.Catch as MC

import Data.Typeable (Typeable)
import Control.Concurrent.MVar
import System.IO.Unsafe (unsafePerformIO)

import Data.IORef
import Data.Maybe

import qualified GHC.Paths

import qualified Hint.GHC as GHC

type Interpreter = InterpreterT IO

newtype InterpreterT m a = InterpreterT {
                             unInterpreterT :: ReaderT InterpreterSession (GHC.GhcT m) a
                           }
    deriving (Functor, Monad, MonadIO, MonadThrow, MonadCatch, MonadMask)

execute :: (MonadIO m, MonadMask m)
        => String
        -> InterpreterSession
        -> InterpreterT m a
        -> m (Either InterpreterError a)
execute libdir s = try
                 . GHC.runGhcT (Just libdir)
                 . flip runReaderT s
                 . unInterpreterT

instance MonadTrans InterpreterT where
    lift = InterpreterT . lift . lift

runGhcImpl :: (MonadIO m, MonadMask m)
           => RunGhc (InterpreterT m) a
runGhcImpl a =
  InterpreterT (lift a)
   `catches`
   [Handler (\(e :: GHC.SourceError)  -> do
     dynFlags <- runGhc GHC.getSessionDynFlags
     throwM $ compilationError dynFlags e)
   ,Handler (\(e :: GHC.GhcApiError)  -> throwM $ GhcException $ show e)
   ,Handler (\(e :: GHC.GhcException) -> throwM $ GhcException $ showGhcEx e)
   ]
  where
    compilationError dynFlags
      = WontCompile
      . map (GhcError . GHC.showSDoc dynFlags)
      . GHC.pprErrMsgBagWithLoc
      . GHC.srcErrorMessages

showGhcEx :: GHC.GhcException -> String
showGhcEx = flip GHC.showGhcException ""

-- ================= Executing the interpreter ==================

initialize :: (MonadIO m, MonadThrow m, MonadMask m, Functor m)
           => [String]
           -> InterpreterT m ()
initialize args =
    do log_handler <- fromSession ghcErrLogger
       -- Set a custom log handler, to intercept error messages :S
       df0 <- runGhc GHC.getSessionDynFlags

       let df1 = configureDynFlags df0
       (df2, extra) <- runGhc2 parseDynamicFlags df1 args
       unless (null extra) $
            throwM $ UnknownError (concat [ "flags: '"
                                          , unwords extra
                                          , "' not recognized"])

       -- Observe that, setSessionDynFlags loads info on packages
       -- available; calling this function once is mandatory!
       _ <- runGhc1 GHC.setSessionDynFlags df2{GHC.log_action = log_handler}

       let extMap      = map (\fs -> (GHC.flagSpecName fs, GHC.flagSpecFlag fs)) GHC.xFlags
       let toOpt e     = let err = error ("init error: unknown ext:" ++ show e)
                         in fromMaybe err (lookup e extMap)
       let getOptVal e = (asExtension e, GHC.xopt (toOpt e) df2)
       let defExts = map  getOptVal supportedExtensions

       onState (\s -> s{defaultExts = defExts})

       reset

-- | Executes the interpreter. Returns @Left InterpreterError@ in case of error.
--
-- NB. In hint-0.7.0 and earlier, the underlying ghc was accidentally
-- overwriting certain signal handlers (SIGINT, SIGHUP, SIGTERM, SIGQUIT on
-- Posix systems, Ctrl-C handler on Windows).
runInterpreter :: (MonadIO m, MonadMask m)
               => InterpreterT m a
               -> m (Either InterpreterError a)
runInterpreter = runInterpreterWithArgs []

-- | Executes the interpreter, setting args passed in as though they
-- were command-line args. Returns @Left InterpreterError@ in case of
-- error.
runInterpreterWithArgs :: (MonadIO m, MonadMask m)
                       => [String]
                       -> InterpreterT m a
                       -> m (Either InterpreterError a)
runInterpreterWithArgs args = runInterpreterWithArgsLibdir args GHC.Paths.libdir

runInterpreterWithArgsLibdir :: (MonadIO m, MonadMask m)
                             => [String]
                             -> String
                             -> InterpreterT m a
                             -> m (Either InterpreterError a)
runInterpreterWithArgsLibdir args libdir action =
  ifInterpreterNotRunning $
    do s <- newInterpreterSession `MC.catch` rethrowGhcException
       execute libdir s (initialize args >> action `finally` cleanSession)
    where rethrowGhcException   = throwM . GhcException . showGhcEx
          newInterpreterSession = newSessionData ()
          cleanSession = cleanPhantomModules

{-# NOINLINE uniqueToken #-}
uniqueToken :: MVar ()
uniqueToken = unsafePerformIO $ newMVar ()

ifInterpreterNotRunning :: (MonadIO m, MonadMask m) => m a -> m a
ifInterpreterNotRunning action =
    do maybe_token <- liftIO $ tryTakeMVar uniqueToken
       case maybe_token of
           Nothing -> throwM MultipleInstancesNotAllowed
           Just x  -> action `finally` liftIO (putMVar uniqueToken x)

-- | The installed version of ghc is not thread-safe. This exception
--   is thrown whenever you try to execute @runInterpreter@ while another
--   instance is already running.
data MultipleInstancesNotAllowed = MultipleInstancesNotAllowed deriving Typeable

instance Exception MultipleInstancesNotAllowed

instance Show MultipleInstancesNotAllowed where
    show _ = "This version of GHC is not thread-safe," ++
             "can't safely run two instances of the interpreter simultaneously"

initialState :: InterpreterState
initialState = St {
                   activePhantoms    = [],
                   zombiePhantoms    = [],
#if defined(NEED_PHANTOM_DIRECTORY)
                   phantomDirectory  = Nothing,
#endif
                   hintSupportModule = error "No support module loaded!",
                   importQualHackMod = Nothing,
                   qualImports       = [],
                   defaultExts       = error "defaultExts missing!",
                   configuration     = defaultConf
                  }

newSessionData :: MonadIO m => a -> m (SessionData a)
newSessionData a =
    do initial_state    <- liftIO $ newIORef initialState
       ghc_err_list_ref <- liftIO $ newIORef []
       return SessionData {
         internalState   = initial_state,
         versionSpecific = a,
         ghcErrListRef   = ghc_err_list_ref,
         ghcErrLogger    = mkLogHandler ghc_err_list_ref
       }

mkLogHandler :: IORef [GhcError] -> GhcErrLogger
mkLogHandler r df _ _ src style msg =
    let renderErrMsg = GHC.showSDoc df
        errorEntry = mkGhcError renderErrMsg src style msg
    in modifyIORef r (errorEntry :)

mkGhcError :: (GHC.SDoc -> String) -> GHC.SrcSpan -> GHC.PprStyle -> GHC.Message -> GhcError
mkGhcError render src_span style msg = GhcError{errMsg = niceErrMsg}
    where niceErrMsg = render . GHC.withPprStyle style $
                         GHC.mkLocMessage GHC.SevError src_span msg

-- The MonadInterpreter instance

instance (MonadIO m, MonadMask m, Functor m) => MonadInterpreter (InterpreterT m) where
    fromSession f = InterpreterT $ asks f
    --
    modifySessionRef target f =
        do ref <- fromSession target
           liftIO $ atomicModifyIORef ref (\a -> (f a, a))
    --
    runGhc = runGhcImpl

instance (Monad m) => Applicative (InterpreterT m) where
    pure  = return
    (<*>) = ap
