{-# LANGUAGE CPP #-}

module Control.Monad.Ghc (
    GhcT, runGhcT
) where

import Control.Applicative
import Prelude

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans as MTL

import Control.Monad.Catch

import Data.IORef

import SysTools
import HscTypes

import qualified GHC
import qualified MonadUtils as GHC
import qualified Exception as GHC
import qualified GhcMonad as GHC

import qualified DynFlags as GHC

import GHCi (stopIServ)

newtype GhcT m a = GhcT { unGhcT :: GHC.GhcT (MTLAdapter m) a }
                 deriving (Functor, Monad, GHC.HasDynFlags)

instance (Functor m, Monad m) => Applicative (GhcT m) where
  pure  = return
  (<*>) = ap

-- adapted from https://github.com/ghc/ghc/blob/ghc-8.2/compiler/main/GHC.hs#L450-L459
-- modified to _not_ catch ^C
rawRunGhcT :: (MonadIO m, MonadMask m) => Maybe FilePath -> GHC.GhcT (MTLAdapter m) a -> MTLAdapter m a
rawRunGhcT mb_top_dir ghct = do
  ref <- liftIO $ newIORef (error "empty session")
  let session = GHC.Session ref
  flip GHC.unGhcT session $ {-GHC.withSignalHandlers $-} do -- do _not_ catch ^C
    GHC.initGhcMonad mb_top_dir
    withCleanupSession ghct

#ifdef GHC801
withCleanupSession :: GHC.GhcMonad m => m a -> m a
withCleanupSession ghc = ghc `GHC.gfinally` cleanup
  where
   cleanup = do
      hsc_env <- GHC.getSession
      let dflags = hsc_dflags hsc_env
      liftIO $ do
          cleanTempFiles dflags
          cleanTempDirs dflags
#ifdef GHCI
          stopIServ hsc_env -- shut down the IServ
#endif /* GHCI */
#else /* GHC801 */
withCleanupSession = GHC.withCleanupSession
#endif /* GHC801 */

runGhcT :: (MonadIO m, MonadMask m) => Maybe FilePath -> GhcT m a -> m a
runGhcT f = unMTLA . rawRunGhcT f . unGhcT

instance MTL.MonadTrans GhcT where
    lift = GhcT . GHC.liftGhcT . MTLAdapter

instance MTL.MonadIO m => MTL.MonadIO (GhcT m) where
    liftIO = GhcT . GHC.liftIO

instance MonadCatch m => MonadThrow (GhcT m) where
    throwM = lift . throwM

instance (MonadIO m, MonadCatch m, MonadMask m) => MonadCatch (GhcT m) where
    m `catch` f = GhcT (unGhcT m `GHC.gcatch` (unGhcT . f))

instance (MonadIO m, MonadMask m) => MonadMask (GhcT m) where
    mask f = wrap $ \s ->
               mask $ \io_restore ->
                 unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
      where
        wrap g   = GhcT $ GHC.GhcT $ \s -> MTLAdapter (g s)
        unwrap m = unMTLA . GHC.unGhcT (unGhcT m)

    uninterruptibleMask f = wrap $ \s ->
                              uninterruptibleMask $ \io_restore ->
                                unwrap (f $ \m -> (wrap $ \s' -> io_restore (unwrap m s'))) s
      where
        wrap g   = GhcT $ GHC.GhcT $ \s -> MTLAdapter (g s)
        unwrap m = unMTLA . GHC.unGhcT (unGhcT m)

    generalBracket acquire release body
      = wrap $ \s -> generalBracket (unwrap acquire s)
                                    (\a exitCase -> unwrap (release a exitCase) s)
                                    (\a -> unwrap (body a) s)
      where
        wrap g   = GhcT $ GHC.GhcT $ \s -> MTLAdapter (g s)
        unwrap m = unMTLA . GHC.unGhcT (unGhcT m)

instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (GhcT m) where
    gcatch  = catch
    gmask f = mask (\x -> f x)

instance (Functor m, MonadIO m, MonadCatch m, MonadMask m) => GHC.GhcMonad (GhcT m) where
    getSession = GhcT GHC.getSession
    setSession = GhcT . GHC.setSession

-- | We use the 'MTLAdapter' to convert between similar classes
--   like 'MTL'''s 'MonadIO' and 'GHC'''s 'MonadIO'.
newtype MTLAdapter m a = MTLAdapter {unMTLA :: m a} deriving (Functor, Applicative, Monad)

instance MTL.MonadIO m => GHC.MonadIO (MTLAdapter m) where
    liftIO = MTLAdapter . MTL.liftIO

instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (MTLAdapter m) where
  m `gcatch` f = MTLAdapter $ unMTLA m `catch` (unMTLA . f)
  gmask io = MTLAdapter $ mask (\f -> unMTLA $ io (MTLAdapter . f . unMTLA))
