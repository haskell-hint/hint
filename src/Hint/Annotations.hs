#if MIN_VERSION_ghc(9,10,0)
{-# LANGUAGE RequiredTypeArguments #-}
#endif
module Hint.Annotations (
    getModuleAnnotations,
    getValAnnotations,
#if MIN_VERSION_ghc(9,10,0)
    getModuleAnnotations',
    getValAnnotations',
#endif
) where

import Data.Data
import GHC.Serialized

import Hint.Base
import qualified Hint.GHC as GHC

#if MIN_VERSION_ghc(9,2,0)
import GHC (ms_mod)
import GHC.Driver.Env (hsc_mod_graph)
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types (hsc_mod_graph, ms_mod)
#else
import HscTypes (hsc_mod_graph, ms_mod)
#endif

#if MIN_VERSION_ghc(9,0,0)
import GHC.Types.Annotations
import GHC.Utils.Monad (concatMapM)
#else
import Annotations
import MonadUtils (concatMapM)
#endif

-- Get the annotations associated with a particular module.
getModuleAnnotations :: forall m a. (Data a, MonadInterpreter m) => a -> String -> m [a]
#if MIN_VERSION_ghc(9,10,0)
getModuleAnnotations _ = getModuleAnnotations' a
#else
getModuleAnnotations _ x = do
    mods <- GHC.mgModSummaries . hsc_mod_graph <$> runGhc GHC.getSession
    let x' = filter ((==) x . GHC.moduleNameString . GHC.moduleName . ms_mod) mods
    concatMapM (anns . ModuleTarget . ms_mod) x'
#endif

#if MIN_VERSION_ghc(9,10,0)
-- Get the annotations associated with a particular module.
getModuleAnnotations' :: MonadInterpreter m => forall a -> Data a => String -> m [a]
getModuleAnnotations' _ x = do
    mods <- GHC.mgModSummaries . hsc_mod_graph <$> runGhc GHC.getSession
    let x' = filter ((==) x . GHC.moduleNameString . GHC.moduleName . ms_mod) mods
    concatMapM (anns . ModuleTarget . ms_mod) x'
#endif

-- Get the annotations associated with a particular function.
getValAnnotations :: forall m a. (Data a, MonadInterpreter m) => a -> String -> m [a]
#if MIN_VERSION_ghc(9,10,0)
getValAnnotations _ = getValAnnotations' a
#else
getValAnnotations _ s = do
    names <- runGhc $ GHC.parseName s
    concatMapM (anns . NamedTarget) names
#endif

#if MIN_VERSION_ghc(9,10,0)
-- Get the annotations associated with a particular function.
getValAnnotations' :: MonadInterpreter m => forall a -> Data a => String -> m [a]
getValAnnotations' _ s = do
    names <- runGhc $ GHC.parseName s
    concatMapM (anns . NamedTarget) names
#endif

anns :: (MonadInterpreter m, Data a) => AnnTarget GHC.Name -> m [a]
anns target = runGhc $ GHC.findGlobalAnns deserializeWithData target
