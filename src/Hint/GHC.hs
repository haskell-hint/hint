{-# LANGUAGE TypeApplications #-}
module Hint.GHC (
    -- * Shims
    dynamicGhc,
    Message,
    Logger,
    WarnReason(NoReason),
    initLogger,
    putLogMsg,
    pushLogHook,
    modifyLogger,
    mkLogAction,
    UnitState,
    emptyUnitState,
    showSDocForUser,
    ParserOpts,
    mkParserOpts,
    initParserState,
    getErrorMessages,
    pprErrorMessages,
    SDocContext,
    defaultSDocContext,
    showGhcException,
    addWay,
    setBackendToInterpreter,
    parseDynamicFlags,
    pprTypeForUser,
    errMsgSpan,
    fileTarget,
    guessTarget,
#if MIN_VERSION_ghc(9,6,0)
    getPrintUnqual,
#endif
    lookupTyCon,
    lookupDataCon,
    -- * Re-exports
    module X,
) where

import Data.IORef (IORef, modifyIORef)

import GHC as X hiding (Phase, GhcT, parseDynamicFlags, runGhcT, showGhcException
                       , guessTarget
#if MIN_VERSION_ghc(9,2,0)
                       , Logger
                       , modifyLogger
                       , pushLogHook
#endif
                       )
import Control.Monad.Ghc as X (GhcT, runGhcT)

#if MIN_VERSION_ghc(9,8,0)
import GHC.Types.Error (ResolvedDiagnosticReason(..), Messages)
import GHC.Driver.Errors.Types (DriverMessage)
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Types.SourceError as X (SourceError, srcErrorMessages)

import GHC.Driver.Ppr as X (showSDoc, showSDocUnsafe)

import GHC.Types.SourceFile as X (HscSource(HsSrcFile))

import GHC.Platform.Ways as X (Way (..))
#elif MIN_VERSION_ghc(9,2,0)
import GHC.Types.SourceError as X (SourceError, srcErrorMessages)

import GHC.Driver.Ppr as X (showSDoc)

import GHC.Types.SourceFile as X (HscSource(HsSrcFile))

import GHC.Platform.Ways as X (Way (..))
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Driver.Types as X (SourceError, srcErrorMessages, GhcApiError)

import GHC.Utils.Outputable as X (showSDoc)

import GHC.Driver.Phases as X (HscSource(HsSrcFile))

import GHC.Driver.Session as X (addWay')
import GHC.Driver.Ways as X (Way (..))
#else
import HscTypes as X (SourceError, srcErrorMessages, GhcApiError)

import Outputable as X (showSDoc)

import DriverPhases as X (HscSource(HsSrcFile))

import DynFlags as X (addWay', Way(..))
#endif

#if MIN_VERSION_ghc(9,4,0)
import GHC.Utils.Outputable as X (PprStyle, SDoc, Outputable(ppr),
                                  withPprStyle, vcat)
import GHC.Driver.Phases as X (Phase(Cpp))
import GHC.Data.StringBuffer as X (stringToStringBuffer)
import GHC.Parser.Lexer as X (P(..), ParseResult(..))
import GHC.Parser as X (parseStmt, parseType)
import GHC.Data.FastString as X (fsLit)

import GHC.Driver.Session as X (xFlags, xopt, FlagSpec(..))

import GHC.Types.Error as X (diagnosticMessage, getMessages)
import GHC.Types.SrcLoc as X (mkRealSrcLoc)

import GHC.Core.ConLike as X (ConLike(RealDataCon))
#elif MIN_VERSION_ghc(9,0,0)
import GHC.Utils.Outputable as X (PprStyle, SDoc, Outputable(ppr),
                                  withPprStyle, vcat)

import GHC.Driver.Phases as X (Phase(Cpp))
import GHC.Data.StringBuffer as X (stringToStringBuffer)
import GHC.Parser.Lexer as X (P(..), ParseResult(..))
import GHC.Parser as X (parseStmt, parseType)
import GHC.Data.FastString as X (fsLit)

import GHC.Driver.Session as X (xFlags, xopt, FlagSpec(..))
import GHC.Driver.Session (WarnReason(NoReason))

import GHC.Types.SrcLoc as X (mkRealSrcLoc)

import GHC.Core.ConLike as X (ConLike(RealDataCon))
#else
import HscTypes as X (mgModSummaries)

import Outputable as X (PprStyle, SDoc, Outputable(ppr),
                        withPprStyle, vcat)

import DriverPhases as X (Phase(Cpp))
import StringBuffer as X (stringToStringBuffer)
import Lexer as X (P(..), ParseResult(..), mkPState)
import Parser as X (parseStmt, parseType)
import FastString as X (fsLit)

import DynFlags as X (xFlags, xopt, FlagSpec(..))

import SrcLoc as X (mkRealSrcLoc)

import ConLike as X (ConLike(RealDataCon))
#endif

{-------------------- Imports for Shims --------------------}

import Control.Monad.IO.Class (MonadIO)

-- guessTarget
import qualified GHC (guessTarget)

-- lookupTyCon, lookupDataCon
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Writer.CPS (execWriterT, tell)
import Data.Foldable (for_)

#if MIN_VERSION_ghc(9,6,0)
-- dynamicGhc
import GHC.Platform.Ways (hostIsDynamic)

-- Message
import qualified GHC.Utils.Error as GHC (mkLocMessage)

-- Logger
import qualified GHC.Utils.Logger as GHC
  (LogAction, Logger, initLogger, logFlags, log_default_user_context, putLogMsg, pushLogHook)
import qualified GHC.Driver.Monad as GHC (modifyLogger)
import qualified GHC.Types.Error as GHC
  (DiagnosticReason(ErrorWithoutFlag), MessageClass(MCDiagnostic))
import qualified GHC.Utils.Outputable as GHC (renderWithContext)

-- UnitState
import qualified GHC.Unit.State as GHC (UnitState, emptyUnitState)

-- showSDocForUser
import qualified GHC.Driver.Ppr as GHC (showSDocForUser)

-- PState
import qualified GHC.Parser.Lexer as GHC (PState, ParserOpts, initParserState)
import GHC.Data.StringBuffer (StringBuffer)
import qualified GHC.Driver.Config.Parser as GHC (initParserOpts)

-- ErrorMessages
import qualified GHC.Driver.Errors.Types as GHC (GhcMessage(GhcPsMessage), ErrorMessages)
import qualified GHC.Parser.Lexer as GHC (getPsErrorMessages)
import qualified GHC.Types.Error as GHC
  (diagnosticMessage, errMsgDiagnostic, getMessages, unDecorated)
import qualified GHC.Types.Error as GHC (defaultDiagnosticOpts)
import GHC.Data.Bag (bagToList)

-- showGhcException
import qualified GHC (showGhcException)
import qualified GHC.Utils.Outputable as GHC (SDocContext, defaultSDocContext)

-- addWay
import qualified GHC.Driver.Session as DynFlags (targetWays_)
import qualified Data.Set as Set

-- parseDynamicFlags
import qualified GHC (parseDynamicFlags)
import GHC.Driver.CmdLine (Warn)

-- pprTypeForUser
import qualified GHC.Core.TyCo.Ppr as GHC (pprSigmaType)

-- errMsgSpan
import qualified GHC.Types.Error as GHC (Messages, errMsgSpan)
import qualified GHC.Types.SrcLoc as GHC (combineSrcSpans)

-- fileTarget
import qualified GHC.Driver.Phases as GHC (Phase(Cpp))
import qualified GHC.Driver.Session as GHC (homeUnitId_)
import qualified GHC.Types.SourceFile as GHC (HscSource(HsSrcFile))
import qualified GHC.Types.Target as GHC (Target(Target), TargetId(TargetFile))

-- getPrintUnqual
import qualified GHC (getNamePprCtx)
#elif MIN_VERSION_ghc(9,4,0)
-- dynamicGhc
import GHC.Platform.Ways (hostIsDynamic)

-- Message
import qualified GHC.Utils.Error as GHC (mkLocMessage)

-- Logger
import qualified GHC.Utils.Logger as GHC
  (LogAction, Logger, initLogger, logFlags, log_default_user_context, putLogMsg, pushLogHook)
import qualified GHC.Driver.Monad as GHC (modifyLogger)
import qualified GHC.Types.Error as GHC
  (DiagnosticReason(ErrorWithoutFlag), MessageClass(MCDiagnostic))
import qualified GHC.Utils.Outputable as GHC (renderWithContext)

-- UnitState
import qualified GHC.Unit.State as GHC (UnitState, emptyUnitState)

-- showSDocForUser
import qualified GHC.Driver.Ppr as GHC (showSDocForUser)

-- PState
import qualified GHC.Parser.Lexer as GHC (PState, ParserOpts, initParserState)
import GHC.Data.StringBuffer (StringBuffer)
import qualified GHC.Driver.Config.Parser as GHC (initParserOpts)

-- ErrorMessages
import qualified GHC.Driver.Errors.Types as GHC (GhcMessage(GhcPsMessage), ErrorMessages)
import qualified GHC.Parser.Lexer as GHC (getPsErrorMessages)
import qualified GHC.Types.Error as GHC
  (diagnosticMessage, errMsgDiagnostic, getMessages, unDecorated)
import GHC.Data.Bag (bagToList)

-- showGhcException
import qualified GHC (showGhcException)
import qualified GHC.Utils.Outputable as GHC (SDocContext, defaultSDocContext)

-- addWay
import qualified GHC.Driver.Session as DynFlags (targetWays_)
import qualified Data.Set as Set

-- parseDynamicFlags
import qualified GHC (parseDynamicFlags)
import GHC.Driver.CmdLine (Warn)

-- pprTypeForUser
import qualified GHC.Core.TyCo.Ppr as GHC (pprSigmaType)

-- errMsgSpan
import qualified GHC.Types.Error as GHC (Messages, errMsgSpan)
import qualified GHC.Types.SrcLoc as GHC (combineSrcSpans)

-- fileTarget
import qualified GHC.Driver.Phases as GHC (Phase(Cpp))
import qualified GHC.Driver.Session as GHC (homeUnitId_)
import qualified GHC.Types.SourceFile as GHC (HscSource(HsSrcFile))
import qualified GHC.Types.Target as GHC (Target(Target), TargetId(TargetFile))

#elif MIN_VERSION_ghc(9,2,0)
-- dynamicGhc
import GHC.Platform.Ways (hostIsDynamic)

-- Message
import qualified GHC.Utils.Error as GHC (mkLocMessage)

-- Logger
import qualified GHC.Utils.Logger as GHC (LogAction, Logger, initLogger, putLogMsg, pushLogHook)
import qualified GHC.Driver.Monad as GHC (modifyLogger)
import qualified GHC.Driver.Ppr as GHC (showSDoc)

-- UnitState
import qualified GHC.Unit.State as GHC (UnitState, emptyUnitState)

-- showSDocForUser
import qualified GHC.Driver.Ppr as GHC (showSDocForUser)

-- PState
import qualified GHC.Parser.Lexer as GHC (PState, ParserOpts, mkParserOpts, initParserState)
import GHC.Data.StringBuffer (StringBuffer)
import qualified GHC.Driver.Session as DynFlags (warningFlags, extensionFlags, safeImportsOn)

-- ErrorMessages
import qualified GHC.Parser.Errors.Ppr as GHC (pprError)
import qualified GHC.Parser.Lexer as GHC (getErrorMessages)
import qualified GHC.Types.Error as GHC (ErrorMessages, errMsgDiagnostic, unDecorated)
import GHC.Data.Bag (bagToList)

-- showGhcException
import qualified GHC (showGhcException)
import qualified GHC.Utils.Outputable as GHC (SDocContext, defaultSDocContext)

-- addWay
import qualified GHC.Driver.Session as DynFlags (targetWays_)
import qualified Data.Set as Set

-- parseDynamicFlags
import qualified GHC (parseDynamicFlags)
import GHC.Driver.CmdLine (Warn)

-- pprTypeForUser
import qualified GHC.Types.TyThing.Ppr as GHC (pprTypeForUser)

-- errMsgSpan
import qualified GHC.Data.Bag as GHC (Bag)
import qualified GHC.Types.Error as GHC (MsgEnvelope, errMsgSpan)
import qualified GHC.Types.SrcLoc as GHC (combineSrcSpans)

-- fileTarget
import qualified GHC.Driver.Phases as GHC (Phase(Cpp))
import qualified GHC.Types.SourceFile as GHC (HscSource(HsSrcFile))
import qualified GHC.Types.Target as GHC (Target(Target), TargetId(TargetFile))

#elif MIN_VERSION_ghc(9,0,0)
-- dynamicGhc
import GHC.Driver.Ways (hostIsDynamic)

-- Message
import qualified GHC.Utils.Error as GHC (MsgDoc, mkLocMessage)

-- Logger
import qualified GHC.Driver.Session as GHC (LogAction, defaultLogAction)
import qualified GHC.Driver.Session as DynFlags (log_action)
import qualified GHC.Utils.Outputable as GHC (showSDoc)

-- showSDocForUser
import qualified GHC.Utils.Outputable as GHC (showSDocForUser)

-- PState
import qualified GHC.Parser.Lexer as GHC (PState, ParserFlags, mkParserFlags, mkPStatePure)
import GHC.Data.StringBuffer (StringBuffer)

-- ErrorMessages
import qualified GHC.Utils.Error as GHC (ErrorMessages, pprErrMsgBagWithLoc)
import qualified GHC.Parser.Lexer as GHC (getErrorMessages)

-- showGhcException
import qualified GHC (showGhcException)

-- addWay
import qualified GHC.Driver.Session as GHC (addWay')

-- parseDynamicFlags
import qualified GHC (parseDynamicFlags)
import GHC.Driver.CmdLine (Warn)

-- pprTypeForUser
import qualified GHC.Core.Ppr.TyThing as GHC (pprTypeForUser)

-- errMsgSpan
import qualified GHC.Types.SrcLoc as GHC (combineSrcSpans)
import qualified GHC.Utils.Error as GHC (errMsgSpan)

-- fileTarget
import qualified GHC.Driver.Phases as GHC (HscSource(HsSrcFile), Phase(Cpp))
import qualified GHC.Driver.Types as GHC (Target(Target), TargetId(TargetFile))

#else
-- dynamicGhc
import qualified DynFlags as GHC (dynamicGhc)

-- Message
import qualified ErrUtils as GHC (MsgDoc, mkLocMessage)

-- Logger
import qualified DynFlags as GHC (LogAction, defaultLogAction)
import qualified DynFlags (log_action)
import DynFlags (WarnReason(NoReason))
import qualified Outputable as GHC (defaultErrStyle, renderWithStyle)

-- showSDocForUser
import qualified Outputable as GHC (showSDocForUser)

-- PState
import qualified Lexer as GHC (PState, ParserFlags, mkParserFlags, mkPStatePure)
import StringBuffer (StringBuffer)

-- ErrorMessages
import qualified ErrUtils as GHC (ErrorMessages, pprErrMsgBagWithLoc)
#if MIN_VERSION_ghc(8,10,0)
import qualified Lexer as GHC (getErrorMessages)
#else
import Bag (emptyBag)
#endif

-- showGhcException
import qualified GHC (showGhcException)

-- addWay
import qualified DynFlags as GHC (addWay')

-- parseDynamicFlags
import qualified GHC (parseDynamicFlags)
import CmdLineParser (Warn)

-- pprTypeForUser
import qualified PprTyThing as GHC (pprTypeForUser)

-- errMsgSpan
import qualified SrcLoc as GHC (combineSrcSpans)
import qualified ErrUtils as GHC (errMsgSpan)

-- fileTarget
import qualified DriverPhases as GHC (HscSource(HsSrcFile), Phase(Cpp))
import qualified HscTypes as GHC (Target(Target), TargetId(TargetFile))

#endif

{-------------------- Shims --------------------}

-- dynamicGhc
dynamicGhc :: Bool
#if MIN_VERSION_ghc(9,0,0)
dynamicGhc = hostIsDynamic
#else
dynamicGhc = GHC.dynamicGhc
#endif

-- Message
#if MIN_VERSION_ghc(9,2,0)
type Message = SDoc
#else
type Message = GHC.MsgDoc
#endif

-- Logger
initLogger :: IO Logger
putLogMsg :: Logger -> DynFlags -> WarnReason -> Severity -> SrcSpan -> SDoc -> IO ()
pushLogHook :: (GHC.LogAction -> GHC.LogAction) -> Logger -> Logger
modifyLogger :: GhcMonad m => (Logger -> Logger) -> m ()
mkLogAction :: (String -> a) -> IORef [a] -> GHC.LogAction
#if MIN_VERSION_ghc(9,8,0)
data WarnReason = NoReason
type Logger = GHC.Logger
initLogger = GHC.initLogger
putLogMsg logger _df _wn sev = GHC.putLogMsg logger (GHC.logFlags logger) (GHC.MCDiagnostic sev (ResolvedDiagnosticReason GHC.ErrorWithoutFlag) Nothing)
pushLogHook = GHC.pushLogHook
modifyLogger = GHC.modifyLogger
mkLogAction f r = \lf mc src msg ->
    let renderErrMsg = GHC.renderWithContext (GHC.log_default_user_context lf)
        errorEntry   = f (renderErrMsg (GHC.mkLocMessage mc src msg))
    in modifyIORef r (errorEntry :)
#elif MIN_VERSION_ghc(9,6,0)
data WarnReason = NoReason
type Logger = GHC.Logger
initLogger = GHC.initLogger
putLogMsg logger _df _wn sev = GHC.putLogMsg logger (GHC.logFlags logger) (GHC.MCDiagnostic sev GHC.ErrorWithoutFlag Nothing)
pushLogHook = GHC.pushLogHook
modifyLogger = GHC.modifyLogger
mkLogAction f r = \lf mc src msg ->
    let renderErrMsg = GHC.renderWithContext (GHC.log_default_user_context lf)
        errorEntry   = f (renderErrMsg (GHC.mkLocMessage mc src msg))
    in modifyIORef r (errorEntry :)
#elif MIN_VERSION_ghc(9,4,0)
data WarnReason = NoReason
type Logger = GHC.Logger
initLogger = GHC.initLogger
putLogMsg logger _df _wn sev = GHC.putLogMsg logger (GHC.logFlags logger) (GHC.MCDiagnostic sev GHC.ErrorWithoutFlag)
pushLogHook = GHC.pushLogHook
modifyLogger = GHC.modifyLogger
mkLogAction f r = \lf mc src msg ->
    let renderErrMsg = GHC.renderWithContext (GHC.log_default_user_context lf)
        errorEntry   = f (renderErrMsg (GHC.mkLocMessage mc src msg))
    in modifyIORef r (errorEntry :)
#elif MIN_VERSION_ghc(9,2,0)
type Logger = GHC.Logger
initLogger = GHC.initLogger
putLogMsg = GHC.putLogMsg
pushLogHook = GHC.pushLogHook
modifyLogger = GHC.modifyLogger
mkLogAction f r = \df _ sev src msg ->
    let renderErrMsg = GHC.showSDoc df
        errorEntry   = f (renderErrMsg (GHC.mkLocMessage sev src msg))
    in modifyIORef r (errorEntry :)
#elif MIN_VERSION_ghc(9,0,0)
type Logger = GHC.LogAction
initLogger = pure GHC.defaultLogAction
putLogMsg = id
pushLogHook = id
modifyLogger f = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df{log_action = f $ DynFlags.log_action df}
  return ()
mkLogAction f r = \df _ sev src msg ->
    let renderErrMsg = GHC.showSDoc df
        errorEntry   = f (renderErrMsg (GHC.mkLocMessage sev src msg))
    in modifyIORef r (errorEntry :)
#else
type Logger = GHC.LogAction
initLogger = pure GHC.defaultLogAction
putLogMsg l = \df wr sev src msg -> l df wr sev src (GHC.defaultErrStyle df) msg
pushLogHook = id
modifyLogger f = do
  df <- getSessionDynFlags
  _ <- setSessionDynFlags df{log_action = f $ DynFlags.log_action df}
  return ()
mkLogAction f r = \df _ sev src style msg ->
    let renderErrMsg s = GHC.renderWithStyle df s style
        errorEntry   = f (renderErrMsg (GHC.mkLocMessage sev src msg))
    in modifyIORef r (errorEntry :)
#endif

-- UnitState
emptyUnitState :: UnitState
#if MIN_VERSION_ghc(9,2,0)
type UnitState = GHC.UnitState
emptyUnitState = GHC.emptyUnitState
#else
type UnitState = ()
emptyUnitState = ()
#endif

-- showSDocForUser
#if MIN_VERSION_ghc(9,6,0)
type PrintUnqualified = NamePprCtx
#endif

showSDocForUser :: DynFlags -> UnitState -> PrintUnqualified -> SDoc -> String
#if MIN_VERSION_ghc(9,2,0)
showSDocForUser = GHC.showSDocForUser
#else
showSDocForUser df _ = GHC.showSDocForUser df
#endif

-- PState
mkParserOpts :: DynFlags -> ParserOpts
initParserState :: ParserOpts -> StringBuffer -> RealSrcLoc -> GHC.PState
#if MIN_VERSION_ghc(9,4,0)
type ParserOpts = GHC.ParserOpts
mkParserOpts = GHC.initParserOpts

initParserState = GHC.initParserState
#elif MIN_VERSION_ghc(9,2,0)
type ParserOpts = GHC.ParserOpts
mkParserOpts =
  -- adapted from
  -- https://hackage.haskell.org/package/ghc-8.10.2/docs/src/Lexer.html#line-2437
  GHC.mkParserOpts
    <$> DynFlags.warningFlags
    <*> DynFlags.extensionFlags
    <*> DynFlags.safeImportsOn
    <*> gopt Opt_Haddock
    <*> gopt Opt_KeepRawTokenStream
    <*> const True

initParserState = GHC.initParserState
#else
type ParserOpts = GHC.ParserFlags
mkParserOpts = GHC.mkParserFlags

initParserState = GHC.mkPStatePure
#endif

-- ErrorMessages
getErrorMessages :: GHC.PState -> DynFlags -> GHC.ErrorMessages
pprErrorMessages :: GHC.ErrorMessages -> [SDoc]
#if MIN_VERSION_ghc(9,6,0)
getErrorMessages pstate _ = fmap GHC.GhcPsMessage $ GHC.getPsErrorMessages pstate
pprErrorMessages = bagToList . fmap pprErrorMessage . GHC.getMessages
  where
    pprErrorMessage = vcat . GHC.unDecorated . GHC.diagnosticMessage (GHC.defaultDiagnosticOpts @GHC.GhcMessage) . GHC.errMsgDiagnostic
#elif MIN_VERSION_ghc(9,4,0)
getErrorMessages pstate _ = fmap GHC.GhcPsMessage $ GHC.getPsErrorMessages pstate
pprErrorMessages = bagToList . fmap pprErrorMessage . GHC.getMessages
  where
    pprErrorMessage = vcat . GHC.unDecorated . GHC.diagnosticMessage . GHC.errMsgDiagnostic
#elif MIN_VERSION_ghc(9,2,0)
getErrorMessages pstate _ = fmap GHC.pprError $ GHC.getErrorMessages pstate
pprErrorMessages = bagToList . fmap pprErrorMessage
  where
    pprErrorMessage = vcat . GHC.unDecorated . GHC.errMsgDiagnostic
#elif MIN_VERSION_ghc(8,10,0)
getErrorMessages = GHC.getErrorMessages
pprErrorMessages = GHC.pprErrMsgBagWithLoc
#else
getErrorMessages _ _ = emptyBag
pprErrorMessages = GHC.pprErrMsgBagWithLoc
#endif

-- SDocContext
defaultSDocContext :: SDocContext
#if MIN_VERSION_ghc(9,2,0)
type SDocContext = GHC.SDocContext
defaultSDocContext = GHC.defaultSDocContext
#else
type SDocContext = ()
defaultSDocContext = ()
#endif

-- showGhcException
showGhcException :: SDocContext -> GhcException -> ShowS
#if MIN_VERSION_ghc(9,2,0)
showGhcException = GHC.showGhcException
#else
showGhcException _ = GHC.showGhcException
#endif

-- addWay
addWay :: Way -> DynFlags -> DynFlags
#if MIN_VERSION_ghc(9,2,0)
addWay way df =
  df
    { targetWays_ = Set.insert way $ DynFlags.targetWays_ df
    }
#else
addWay = GHC.addWay'
#endif

-- setBackendToInterpreter
setBackendToInterpreter :: DynFlags -> DynFlags
#if MIN_VERSION_ghc(9,6,0)
setBackendToInterpreter df = df{backend = interpreterBackend}
#elif MIN_VERSION_ghc(9,2,0)
setBackendToInterpreter df = df{backend = Interpreter}
#else
setBackendToInterpreter df = df{hscTarget = HscInterpreted}
#endif

-- parseDynamicFlags

#if MIN_VERSION_ghc(9,8,0)
parseDynamicFlags :: MonadIO m => Logger -> DynFlags -> [Located String] -> m (DynFlags, [Located String], Messages DriverMessage)
parseDynamicFlags = GHC.parseDynamicFlags
#else
#if MIN_VERSION_ghc(9,2,0)
parseDynamicFlags :: MonadIO m => Logger -> DynFlags -> [Located String] -> m (DynFlags, [Located String], [Warn])
parseDynamicFlags = GHC.parseDynamicFlags
#else
parseDynamicFlags _ = GHC.parseDynamicFlags
#endif
#endif

pprTypeForUser :: Type -> SDoc
#if MIN_VERSION_ghc(9,4,0)
pprTypeForUser = GHC.pprSigmaType
#else
pprTypeForUser = GHC.pprTypeForUser
#endif

#if MIN_VERSION_ghc(9,4,0)
errMsgSpan :: GHC.Messages e -> SrcSpan
errMsgSpan msgs = foldr (GHC.combineSrcSpans . GHC.errMsgSpan) X.noSrcSpan (GHC.getMessages msgs)
#elif MIN_VERSION_ghc(9,2,0)
errMsgSpan :: GHC.Bag (GHC.MsgEnvelope e) -> SrcSpan
errMsgSpan = foldr (GHC.combineSrcSpans . GHC.errMsgSpan) X.noSrcSpan
#else
errMsgSpan :: GHC.ErrorMessages -> SrcSpan
errMsgSpan = foldr (GHC.combineSrcSpans . GHC.errMsgSpan) X.noSrcSpan
#endif

fileTarget :: DynFlags -> FilePath -> GHC.Target
#if MIN_VERSION_ghc(9,4,0)
fileTarget dflags f = GHC.Target (GHC.TargetFile f $ Just next_phase) True uid Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile
          uid = GHC.homeUnitId_ dflags
#else
fileTarget _ f = GHC.Target (GHC.TargetFile f $ Just next_phase) True Nothing
    where next_phase = GHC.Cpp GHC.HsSrcFile
#endif

guessTarget :: GhcMonad m => String -> Maybe GHC.Phase -> m GHC.Target
#if MIN_VERSION_ghc(9,4,0)
guessTarget t pM = GHC.guessTarget t Nothing pM
#else
guessTarget = GHC.guessTarget
#endif

-- getPrintUnqual
#if MIN_VERSION_ghc(9,6,0)
getPrintUnqual :: GhcMonad m => m PrintUnqualified
getPrintUnqual = GHC.getNamePprCtx
#endif

lookupCandidates :: GhcMonad m => String -> String -> m [Name]
lookupCandidates moduleString name = do
  parseName (moduleString ++ "." ++ name)

lookupTyCon :: GhcMonad m => String -> String -> m (Either String TyCon)
lookupTyCon moduleString tyConString = do
  names <- lookupCandidates moduleString tyConString
  tyCons <- execWriterT $ do
    for_ names $ \name -> do
      maybeTyThing <- lift $ lookupName name
      case maybeTyThing of
        Just (ATyCon tyCon) -> do
          tell [tyCon]
        _ -> do
          pure ()
  case tyCons of
    [tyCon] -> do
      pure $ Right tyCon
    [] -> do
      pure $ Left $ "Could not find type constructor " ++ tyConString
    _ -> do
      pure $ Left $ "Ambiguous type constructor " ++ tyConString

lookupDataCon :: GhcMonad m => String -> String -> m (Either String DataCon)
lookupDataCon moduleString dataConString = do
  names <- lookupCandidates moduleString dataConString
  dataCons <- execWriterT $ do
    for_ names $ \name -> do
      maybeTyThing <- lift $ lookupName name
      case maybeTyThing of
        Just (AConLike (RealDataCon dataCon)) -> do
          tell [dataCon]
        _ -> do
          pure ()
  case dataCons of
    [dataCon] -> do
      pure $ Right dataCon
    [] -> do
      pure $ Left $ "Could not find data constructor " ++ dataConString
    _ -> do
      pure $ Left $ "Ambiguous data constructor " ++ dataConString