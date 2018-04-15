module Hint.Parsers where

import Prelude hiding (span)

import Hint.Base

import Control.Monad.Trans (liftIO)

import qualified Hint.GHC as GHC

data ParseResult = ParseOk | ParseError GHC.SrcSpan GHC.Message

parseExpr :: MonadInterpreter m => String -> m ParseResult
parseExpr = runParser GHC.parseStmt

parseType :: MonadInterpreter m => String -> m ParseResult
parseType = runParser GHC.parseType

runParser :: MonadInterpreter m => GHC.P a -> String -> m ParseResult
runParser parser expr =
    do dyn_fl <- runGhc GHC.getSessionDynFlags
       --
       buf <- (return . GHC.stringToStringBuffer) expr
       --
       -- ghc >= 7 panics if noSrcLoc is given
       let srcLoc = GHC.mkRealSrcLoc (GHC.fsLit "<hint>") 1 1
       let parse_res = GHC.unP parser (GHC.mkPState dyn_fl buf srcLoc)
       --
       case parse_res of
           GHC.POk{}            -> return ParseOk
           --
#if __GLASGOW_HASKELL__ >= 804
           GHC.PFailed _ span err
#else
           GHC.PFailed span err
#endif
                                -> return (ParseError span err)

failOnParseError :: MonadInterpreter m
                 => (String -> m ParseResult)
                 -> String
                 -> m ()
failOnParseError parser expr = mayFail go
    where go = do parsed <- parser expr
                  --
                  -- If there was a parsing error,
                  -- do the "standard" error reporting
                  case parsed of
                      ParseOk             -> return (Just ())
                      --
                      ParseError span err ->
                          do -- parsing failed, so we report it just as all
                             -- other errors get reported....
                             logger <- fromSession ghcErrLogger
                             dflags <- runGhc GHC.getSessionDynFlags
                             let logger'  = logger dflags
                                 errStyle = GHC.defaultErrStyle dflags
                             liftIO $ logger'
                                              GHC.NoReason
                                              GHC.SevError
                                              span
                                              errStyle
                                              err
                             --
                             -- behave like the rest of the GHC API functions
                             -- do on error...
                             return Nothing
