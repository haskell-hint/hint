module Hint.Annotations (
    getModuleAnnotations,
    getValAnnotations
) where

import Data.Data
import Annotations
import GHC.Serialized

import Hint.Base
import HscTypes (hsc_mod_graph, ms_mod)
import qualified Hint.GHC as GHC

-- Get the annotations associated with a particular module.
getModuleAnnotations :: (Data a, MonadInterpreter m) => a -> String -> m [a]
getModuleAnnotations _ x = do
    mods <- fmap (GHC.mgModSummaries . hsc_mod_graph) $ runGhc GHC.getSession
    let x' = filter ((==) x . GHC.moduleNameString . GHC.moduleName . ms_mod) mods
    v <- mapM (anns . ModuleTarget . ms_mod) x'
    return $ concat v

-- Get the annotations associated with a particular function.
getValAnnotations :: (Data a, MonadInterpreter m) => a -> String -> m [a]
getValAnnotations _ x = do
    x' <- runGhc1 GHC.parseName x
    v <- mapM (anns . NamedTarget) x'
    return $ concat v

anns :: (MonadInterpreter m, Data a) => AnnTarget GHC.Name -> m [a]
anns = runGhc1 (GHC.findGlobalAnns deserializeWithData)
