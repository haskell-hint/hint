module Hint.Base (
    MonadInterpreter(..), RunGhc,

    GhcError(..), InterpreterError(..), mayFail, catchIE,

    InterpreterSession, SessionData(..),
    InterpreterState(..), fromState, onState,
    InterpreterConfiguration(..),
    ImportList(..), ModuleQualification(..), ModuleImport(..),

    ModuleName, PhantomModule(..),
    findModule, moduleIsLoaded,
    withDynFlags,

    ghcVersion,

    debug, showGHC
) where

import Control.Monad.IO.Class
import Control.Monad.Catch as MC

import Data.IORef
#if !MIN_VERSION_ghc(9,12,0)
import Data.Dynamic
#endif
import qualified Data.List

import qualified Hint.GHC as GHC

import Hint.Extension

-- | Version of the underlying ghc api. Values are:
--
-- * @804@ for GHC 8.4.x
--
-- * @806@ for GHC 8.6.x
--
-- * etc...
ghcVersion :: Int
ghcVersion = __GLASGOW_HASKELL__

class (MonadIO m, MonadMask m) => MonadInterpreter m where
    fromSession      :: FromSession m a
    modifySessionRef :: ModifySessionRef m a
    runGhc           :: RunGhc m a

-- this is for hiding the actual types in haddock
type FromSession      m a = (InterpreterSession -> a) -> m a
type ModifySessionRef m a = (InterpreterSession -> IORef a) -> (a -> a) -> m a

data InterpreterError = UnknownError String
                      | WontCompile [GhcError]
                      | NotAllowed  String
                      -- | GhcExceptions from the underlying GHC API are caught
                      -- and rethrown as this.
                      | GhcException String
#if MIN_VERSION_ghc(9,12,0)
                      deriving (Show)
#else
                      deriving (Show, Typeable)
#endif

data InterpreterState = St {
                           activePhantoms    :: [PhantomModule],
                           zombiePhantoms    :: [PhantomModule],
                           phantomDirectory  :: Maybe FilePath,
                           hintSupportModule :: PhantomModule,
                           importQualHackMod :: Maybe PhantomModule,
                           qualImports       :: [ModuleImport],
                           defaultExts       :: [(Extension, Bool)], -- R/O
                           configuration     :: InterpreterConfiguration
                        }

data ImportList = NoImportList | ImportList [String] | HidingList [String]
  deriving (Eq, Show)
data ModuleQualification = NotQualified | ImportAs String | QualifiedAs (Maybe String)
  deriving (Eq, Show)

-- | Represent module import statement.
--   See 'setImportsF'
data ModuleImport = ModuleImport { modName :: String
                                 , modQual :: ModuleQualification
                                 , modImp  :: ImportList
                                 } deriving (Show)

data InterpreterConfiguration = Conf {
                                  searchFilePath :: [FilePath],
                                  languageExts   :: [Extension],
                                  allModsInScope :: Bool
                                }

type InterpreterSession = SessionData ()

instance Exception InterpreterError
  where
    displayException (UnknownError err) = "UnknownError: " ++ err
    displayException (WontCompile  es)  = unlines . Data.List.nub . map errMsg $ es
    displayException (NotAllowed   err) = "NotAllowed: "   ++ err
    displayException (GhcException err) = "GhcException: " ++ err

type RunGhc  m a =
    (forall n.(MonadIO n, MonadMask n) => GHC.GhcT n a)
 -> m a

data SessionData a = SessionData {
                       internalState   :: IORef InterpreterState,
                       versionSpecific :: a,
                       ghcErrListRef   :: IORef [GhcError],
                       ghcLogger       :: GHC.Logger
                     }

-- When intercepting errors reported by GHC, we only get a ErrUtils.Message
-- and a SrcLoc.SrcSpan. The latter holds the file name and the location
-- of the error. However, SrcSpan is abstract and it doesn't provide
-- functions to retrieve the line and column of the error... we can only
-- generate a string with this information. Maybe I can parse this string
-- later.... (sigh)
newtype GhcError = GhcError{errMsg :: String} deriving Show

mapGhcExceptions :: MonadInterpreter m
                 => (String -> InterpreterError)
                 -> m a
                 -> m a
mapGhcExceptions buildEx action =
    action
      `MC.catch` (\err -> case err of
                            GhcException s -> throwM (buildEx s)
                            _              -> throwM err)

catchIE :: MonadInterpreter m => m a -> (InterpreterError -> m a) -> m a
catchIE = MC.catch

-- | Module names are _not_ filepaths.
type ModuleName = String

-- ================ Handling the interpreter state =================

fromState :: MonadInterpreter m => (InterpreterState -> a) -> m a
fromState f = do
  ref_st <- fromSession internalState
  liftIO $ f <$> readIORef ref_st

onState :: MonadInterpreter m => (InterpreterState -> InterpreterState) -> m ()
onState f = () <$ modifySessionRef internalState f

-- =============== Error handling ==============================

mayFail :: MonadInterpreter m => m (Maybe a) -> m a
mayFail action =
    do
        maybe_res <- action
        --
        es <- modifySessionRef ghcErrListRef (const [])
        --
        case (maybe_res, null es) of
            (Nothing, True)  -> throwM $ UnknownError "Got no error message"
            (Nothing, False) -> throwM $ WontCompile (reverse es)
            (Just a, _)      -> return a

-- ================= Debugging stuff ===============

debug :: MonadInterpreter m => String -> m ()
debug = liftIO . putStrLn . ("!! " ++)

showGHC :: (MonadInterpreter m, GHC.Outputable a) => a -> m String
showGHC a
 = do unqual <- runGhc GHC.getPrintUnqual
      withDynFlags $ \df ->
        -- TODO: get unit state from somewhere?
        return $ GHC.showSDocForUser df GHC.emptyUnitState unqual (GHC.ppr a)

-- ================ Misc ===================================

-- this type ought to go in Hint.Context, but ghc dislikes cyclic imports...
data PhantomModule = PhantomModule{pmName :: ModuleName, pmFile :: FilePath}
                   deriving (Eq, Show)

findModule :: MonadInterpreter m => ModuleName -> m GHC.Module
findModule mn = mapGhcExceptions NotAllowed $
                    runGhc $ GHC.findModule mod_name Nothing
    where mod_name = GHC.mkModuleName mn

moduleIsLoaded :: MonadInterpreter m => ModuleName -> m Bool
moduleIsLoaded mn = (True <$ findModule mn)
                   `catchIE` (\e -> case e of
                                      NotAllowed{}  -> return False
                                      WontCompile{} -> return False
                                      _             -> throwM e)

withDynFlags :: MonadInterpreter m => (GHC.DynFlags -> m a) -> m a
withDynFlags action = do
  df <- runGhc GHC.getSessionDynFlags
  action df
