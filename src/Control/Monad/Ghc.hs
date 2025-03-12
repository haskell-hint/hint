module Control.Monad.Ghc (
    GhcT, runGhcT
) where

import Control.Applicative
import Prelude

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class

import Control.Monad.Catch

import Data.IORef

import qualified GHC
#if MIN_VERSION_ghc(9,2,0)
import qualified GHC.Utils.Logger as GHC
#endif
#if MIN_VERSION_ghc(9,0,0)
import qualified GHC.Utils.Monad as GHC
import qualified GHC.Utils.Exception as GHC
import qualified GHC.Driver.Monad as GHC

import qualified GHC.Driver.Session as GHC
#else
import qualified MonadUtils as GHC
import qualified Exception as GHC
import qualified GhcMonad as GHC

import qualified DynFlags as GHC
#endif

newtype GhcT m a = GhcT { unGhcT :: GHC.GhcT (MTLAdapter m) a }
                 deriving (Functor, Monad, GHC.HasDynFlags)

instance (Functor m, Monad m) => Applicative (GhcT m) where
  pure  = GhcT . pure
  (<*>) = ap

-- adapted from https://github.com/ghc/ghc/blob/ghc-8.2/compiler/main/GHC.hs#L450-L459
-- modified to _not_ catch ^C
rawRunGhcT :: (MonadIO m, MonadMask m) => Maybe FilePath -> GHC.GhcT (MTLAdapter m) a -> MTLAdapter m a
rawRunGhcT mb_top_dir ghct = do
  ref <- liftIO $ newIORef (error "empty session")
  let session = GHC.Session ref
  flip GHC.unGhcT session $ {-GHC.withSignalHandlers $-} do -- do _not_ catch ^C
    GHC.initGhcMonad mb_top_dir
    GHC.withCleanupSession ghct

runGhcT :: (MonadIO m, MonadMask m) => Maybe FilePath -> GhcT m a -> m a
runGhcT f = unMTLA . rawRunGhcT f . unGhcT

instance MonadTrans GhcT where
    lift = GhcT . GHC.liftGhcT . MTLAdapter

instance MonadIO m => MonadIO (GhcT m) where
    liftIO = GhcT . GHC.liftIO

instance MonadCatch m => MonadThrow (GhcT m) where
    throwM = lift . throwM

instance (MonadIO m, MonadCatch m, MonadMask m) => MonadCatch (GhcT m) where
#if MIN_VERSION_ghc(9,0,0)
    m `catch` f = GhcT (unGhcT m `catch` (unGhcT . f))
#else
    m `catch` f = GhcT (unGhcT m `GHC.gcatch` (unGhcT . f))
#endif

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

#if !MIN_VERSION_ghc(9,0,0)
instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (GhcT m) where
    gcatch = catch
    gmask  = mask
#endif

#if MIN_VERSION_ghc(9,2,0)
instance MonadIO m => GHC.HasLogger (GhcT m) where
    getLogger = GhcT GHC.getLogger
#endif

instance (Functor m, MonadIO m, MonadCatch m, MonadMask m) => GHC.GhcMonad (GhcT m) where
    getSession = GhcT GHC.getSession
    setSession = GhcT . GHC.setSession

-- | We use the 'MTLAdapter' to convert between similar classes
--   like 'MTL'''s 'MonadIO' and 'GHC'''s 'MonadIO'.
newtype MTLAdapter m a = MTLAdapter {unMTLA :: m a} deriving (Functor, Applicative, Monad)

instance MonadIO m => GHC.MonadIO (MTLAdapter m) where
    liftIO = MTLAdapter . liftIO

#if MIN_VERSION_ghc(9,0,0)
instance MonadCatch m => MonadCatch (MTLAdapter m) where
  m `catch` f = MTLAdapter $ unMTLA m `catch` (unMTLA . f)

instance MonadMask m => MonadMask (MTLAdapter m) where
  mask io = MTLAdapter $ mask (\f -> unMTLA $ io (MTLAdapter . f . unMTLA))
  uninterruptibleMask f = MTLAdapter (unMTLA (uninterruptibleMask f))
  generalBracket acquire release body
    = MTLAdapter (generalBracket (unMTLA acquire)
                                 (\a exitCase -> unMTLA (release a exitCase))
                                 (unMTLA . body))

instance MonadThrow m => MonadThrow (MTLAdapter m) where
  throwM = MTLAdapter . throwM
#else
instance (MonadIO m, MonadCatch m, MonadMask m) => GHC.ExceptionMonad (MTLAdapter m) where
  m `gcatch` f = MTLAdapter $ unMTLA m `catch` (unMTLA . f)
  gmask io = MTLAdapter $ mask (\f -> unMTLA $ io (MTLAdapter . f . unMTLA))
#endif
