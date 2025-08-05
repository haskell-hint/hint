module Hint.Configuration (
      setGhcOption, setGhcOptions,

      defaultConf,

      get, set, Option, OptionVal(..),

      languageExtensions, availableExtensions, Extension(..),
      installedModulesInScope,

      searchPath,

      configureDynFlags, parseDynamicFlags,

) where

import Control.Monad
import Control.Monad.Catch
import Data.Char
#if !MIN_VERSION_ghc(9,12,0)
import Data.Maybe (maybe)
#endif
import Data.List (intercalate)

import qualified Hint.GHC as GHC
import Hint.Base
import Hint.Util (quote)

import Hint.Extension

setGhcOptions :: MonadInterpreter m => [String] -> m ()
setGhcOptions opts =
    do old_flags <- runGhc GHC.getSessionDynFlags
       logger <- fromSession ghcLogger
       (new_flags,not_parsed) <- runGhc $ parseDynamicFlags logger old_flags opts
       unless (null not_parsed) $
            throwM $ UnknownError
                            $ concat ["flags: ", unwords $ map quote not_parsed,
                                               "not recognized"]
       _ <- runGhc $ GHC.setSessionDynFlags new_flags
       return ()

setGhcOption :: MonadInterpreter m => String -> m ()
setGhcOption opt = setGhcOptions [opt]

defaultConf :: InterpreterConfiguration
defaultConf = Conf {
                languageExts   = [],
                allModsInScope = False,
                searchFilePath = ["."]
              }

-- | Available options are:
--
--    * 'languageExtensions'
--
--    * 'installedModulesInScope'
--
--    * 'searchPath'
data Option m a = Option{
                    _set :: MonadInterpreter m => a -> m (),
                    _get :: MonadInterpreter m => m a
                  }

data OptionVal m = forall a . (Option m a) := a

-- | Use this function to set or modify the value of any option. It is
--   invoked like this:
--
--   @set [opt1 := val1, opt2 := val2,... optk := valk]@
set :: MonadInterpreter m => [OptionVal m] -> m ()
set = mapM_ $ \(opt := val) -> _set opt val

-- | Retrieves the value of an option.
get :: MonadInterpreter m => Option m a -> m a
get = \o -> _get o

-- | Language extensions in use by the interpreter.
--
-- Default is: @[]@ (i.e. none, pure Haskell 98)
languageExtensions :: MonadInterpreter m => Option m [Extension]
languageExtensions = Option setter getter
    where setter es = do resetExtensions
                         setGhcOptions $ map (extFlag True) es
                         onConf $ \c -> c{languageExts = es}
          --
          getter = fromConf languageExts
          --
          resetExtensions = do es <- fromState defaultExts
                               setGhcOptions $ uncurry (flip extFlag) <$> es

extFlag :: Bool -> Extension -> String
extFlag = mkFlag
  where mkFlag b (UnknownExtension o)   = strToFlag b o
        mkFlag b o                      = strToFlag b (show o)
        --
        strToFlag b o@('N':'o':(c:_))
                             | isUpper c = "-X" ++ drop (if b then 0 else 2) o
        strToFlag b o                    = "-X" ++ concat ["No"|not b] ++ o

-- | When set to @True@, every module in every available package is implicitly
--   imported qualified. This is very convenient for interactive
--   evaluation, but can be a problem in sandboxed environments
--   (e.g. 'System.Unsafe.unsafePerformIO' is in scope).
--
--   Default value is @True@.
--
--   Observe that due to limitations in the GHC-API, when set to @False@, the
--   private symbols in interpreted modules will not be in scope.
installedModulesInScope :: MonadInterpreter m => Option m Bool
installedModulesInScope = Option setter getter
    where getter = fromConf allModsInScope
          setter b = do onConf $ \c -> c{allModsInScope = b}
                        setGhcOption $ "-f"                   ++
                                       concat ["no-" | not b] ++
                                       "implicit-import-qualified"

-- | The search path for source files. Observe that every time it is set,
--   it overrides the previous search path. The default is @[\".\"]@.
--
--   Keep in mind that by a limitation in ghc, @\".\"@ is always in scope.
searchPath :: MonadInterpreter m => Option m [FilePath]
searchPath = Option setter getter
    where getter = fromConf searchFilePath
          setter p = do onConf $ \c -> c{searchFilePath = p}
                        setGhcOption "-i" -- clear the old path
                        setGhcOption $ "-i" ++ intercalate ":" p

                        mfp <- fromState phantomDirectory
                        maybe (return ())
                              (\fp -> setGhcOption $ "-i" ++ fp) mfp

fromConf :: MonadInterpreter m => (InterpreterConfiguration -> a) -> m a
fromConf f = fromState (f . configuration)

onConf :: MonadInterpreter m
       => (InterpreterConfiguration -> InterpreterConfiguration)
       -> m ()
onConf f = onState $ \st -> st{configuration = f (configuration st)}

configureDynFlags :: GHC.DynFlags -> GHC.DynFlags
configureDynFlags dflags =
    (if GHC.dynamicGhc then GHC.addWay GHC.WayDyn else id)
    . GHC.setBackendToInterpreter
    $
                           dflags{GHC.ghcMode    = GHC.CompManager,
                                  GHC.ghcLink    = GHC.LinkInMemory,
                                  GHC.verbosity  = 0}

parseDynamicFlags :: GHC.GhcMonad m
                  => GHC.Logger -> GHC.DynFlags -> [String] -> m (GHC.DynFlags, [String])
parseDynamicFlags l d = fmap firstTwo . GHC.parseDynamicFlags l d . map GHC.noLoc
    where firstTwo (a,b,_) = (a, map GHC.unLoc b)
