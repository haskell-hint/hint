module Main (main) where

import Prelude hiding (catch)

import Control.Exception (ArithException(..), AsyncException(UserInterrupt))
import Control.Monad.Catch as MC

import Control.Monad (guard, liftM, when, void, (>=>))

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM

import qualified Data.ByteString.Lazy as ByteString
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Function ((&))
import Data.IORef

import System.IO
import System.FilePath
import System.Directory
import System.Environment (getEnvironment, unsetEnv)
import System.Exit
import System.Process.Typed
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
#else
import System.Posix.Signals
#endif

import Test.HUnit ((@?=), (@?), assertFailure)
import qualified Test.HUnit as HUnit

import Language.Haskell.Interpreter
import Language.Haskell.Interpreter.Unsafe

test_reload_modified :: TestCase
test_reload_modified = TestCase "reload_modified" [mod_file] $ do
                            liftIO $ writeFile mod_file mod_v1
                            f_v1 <- get_f
                            --
                            liftIO $ writeFile mod_file mod_v2
                            f_v2 <- get_f
                            --
                            liftIO $ (f_v1 5, f_v2 5) @?= (5, 6)
    --
    where mod_name = "TEST_ReloadModified"
          mod_file = mod_name ++ ".hs"
          --
          mod_v1   = unlines ["module " ++ mod_name,
                              "where",
                              "f :: Int -> Int",
                              "f = id"]
          mod_v2   = unlines ["module " ++ mod_name,
                              "where",
                              "f :: Int -> Int",
                              "f = (1 +)"]
          --
          get_f    = do loadModules [mod_file]
                        setTopLevelModules [mod_name]
                        interpret "f" (as :: Int -> Int)

test_lang_exts :: TestCase
test_lang_exts = TestCase "lang_exts" [mod_file] $ do
                      liftIO $ writeFile mod_file $ unlines
                        [ "data Foo = Foo { a :: Int }"
                        , "f Foo{..} = a * 10"
                        ]
                      fails do_load @@? "first time, it shouldn't load"
                      --
                      set [languageExtensions := [RecordWildCards]]
                      succeeds do_load @@? "now, it should load"
                      --
                      set [languageExtensions := []]
                      fails do_load @@? "it shouldn't load, again"
    --
    where mod_name = "TEST_LangExts"
          mod_file = mod_name ++ ".hs"
          --
          do_load  = loadModules [mod_name]

test_work_in_main :: TestCase
test_work_in_main = TestCase "work_in_main" [mod_file] $ do
                        liftIO $ writeFile mod_file "f = id"
                        loadModules [mod_file]
                        setTopLevelModules ["Main"]
                        setImportsQ [("Prelude", Nothing),
                                     ("Data.Maybe", Just "Mb")]
                        --
                        typeOf "f $ (1 + 1 :: Int)" @@?= "Int"
                        eval "f . Mb.fromJust $ Just [1,2]" @@?= "[1,2]"
                        interpret "f $ 1 == 2" infer @@?= False
    --
    where mod_file     = "TEST_WorkInMain.hs"

test_priv_syms_in_scope :: TestCase
test_priv_syms_in_scope = TestCase "private_syms_in_scope" [mod_file] $ do
                               -- must set to True, otherwise won't work with
                               -- ghc 6.8
                               set [installedModulesInScope := True]
                               liftIO $ writeFile mod_file mod_text
                               loadModules [mod_file]
                               setTopLevelModules ["T"]
                               typeChecks "g" @@? "g is hidden"
    where mod_text = unlines ["module T(f) where", "f = g", "g = id"]
          mod_file = "TEST_PrivateSymbolsInScope.hs"

test_comments_in_expr :: TestCase
test_comments_in_expr = TestCase "comments_in_expr" [] $ do
                            setImports ["Prelude"]
                            let expr = "length $ concat [[1,2],[3]] -- bla"
                            typeChecks expr @@? "comment on expression"
                            _ <- eval expr
                            _ <- interpret expr (as :: Int)
                            return ()

test_qual_import :: TestCase
test_qual_import = TestCase "qual_import" [] $ do
                           setImportsQ [("Prelude", Nothing),
                                        ("Data.Map", Just "M")]
                           typeChecks "null []" @@? "Unqual null"
                           typeChecks "M.null M.empty" @@? "Qual null"

test_full_import :: TestCase
test_full_import = TestCase "full_import" [] $ do
                           setImportsF [ ModuleImport "Prelude" (QualifiedAs Nothing) NoImportList
                                       , ModuleImport "Data.List" (QualifiedAs $ Just "List") $ ImportList ["null"]
                                       ]
                           typeChecks "Prelude.null []" @@? "Qual prelude null"
                           typeChecks "List.null []" @@? "Qual list null"

test_basic_eval :: TestCase
test_basic_eval = TestCase "basic_eval" [] $ eval "()" @@?= "()"

test_eval_layout :: TestCase
test_eval_layout = TestCase "eval_layout" [] $ eval layout_expr @@?= "10"
    where layout_expr = unlines ["let x = let y = 10",
                                 "        in y",
                                 "in x"]

test_show_in_scope :: TestCase
test_show_in_scope = TestCase "show_in_scope" [] $ do
                       setImports ["Prelude"]
                       eval "show ([] :: String)" @@?= show (show "")

test_installed_not_in_scope :: TestCase
test_installed_not_in_scope = TestCase "installed_not_in_scope" [] $ do
                                b <- get installedModulesInScope
                                succeeds action @@?= b
                                set [installedModulesInScope := False]
                                fails action @@? "now must be out of scope"
                                set [installedModulesInScope := True]
                                succeeds action @@? "must be in scope again"
    where action = typeOf "Data.Map.singleton"

test_search_path :: TestCase
test_search_path =
    TestCase "search_path" files $ do
           liftIO setup
           fails (loadModules [mod_1]) @@? "mod_1 should not be in path (1)"
           fails (loadModules [mod_2]) @@? "mod_2 should not be in path (1)"
           --
           set [searchPath := [dir_1]]
           succeeds (loadModules [mod_1]) @@? "mod_1 should be in path (2)"
           fails    (loadModules [mod_2]) @@? "mod_2 should not be in path (2)"
           --
           set [searchPath := [dir_2]]
           fails    (loadModules [mod_1]) @@? "mod_1 should not be in path (3)"
           succeeds (loadModules [mod_2]) @@? "mod_2 should be in path (3)"
           --
           set [searchPath := [dir_1,dir_2]]
           succeeds (loadModules [mod_1]) @@? "mod_1 should be in path (4)"
           succeeds (loadModules [mod_2]) @@? "mod_2 should be in path (4)"
    where dir_1  = "search_path_test_dir_1"
          mod_1  = "M1"
          file_1 = dir_1 </> mod_1 <.> "hs"
          dir_2  = "search_path_test_dir_2"
          mod_2  = "M2"
          file_2 = dir_2 </> mod_2 <.> "hs"
          files  = [file_1, file_2, dir_1, dir_2]
          setup  = do createDirectory dir_1
                      createDirectory dir_2
                      writeFile file_1 $
                         unlines ["module " ++ mod_1,
                                  "where",
                                  "x :: Int",
                                  "x = 42"]
                      writeFile file_2 $
                         unlines ["module " ++ mod_2,
                                  "where",
                                  "y :: Bool",
                                  "y = False"]

test_search_path_dot :: TestCase
test_search_path_dot =
    TestCase "search_path_dot" [mod_file, dir] $ do
           liftIO setup
           succeeds (loadModules [mod1]) @@? "mod1 must be initially in path"
           set [searchPath := [dir]]
           succeeds (loadModules [mod1]) @@? "mod1 must be still in path"
    --
    where dir      = "search_path_dot_dir"
          mod1     = "M1"
          mod_file = mod1 <.> "hs"
          setup    = do createDirectory dir
                        writeFile mod_file $
                           unlines ["x :: Int", "x = 42"]

test_catch :: TestCase
test_catch = TestCase "catch" [] $ do
        setImports ["Prelude"]

        -- make sure we catch exceptions in return, and that the interpreter is
        -- still in a good state afterwards
        explosiveThunk <- eval "1 `div` 0 :: Int"
        (detonate explosiveThunk `catch` handleDivideByZero) @@?= "caught"
        eval "2 + 2 :: Int"  @@?= "4"

        -- make sure we catch exceptions in eval, and that the interpreter is
        -- still in a good state afterwards
        (eval "2 +" `catch` handleWontCompile) @@?= "caught"
        eval "2 + 2 :: Int"  @@?= "4"

        -- make sure we catch exceptions in setImports, and that the interpreter
        -- is still in a good state afterwards
        (importNonsense `catch` handleWontCompile) @@?= "caught"
        eval "2 + 2 :: Int"  @@?= "4"

        -- make sure we catch exceptions in loadModules
        (loadNonsense `catch` handleWontCompile) @@?= "caught"
        
        -- loadModules resets the interpreter state, so we do _not_ expect that
        -- the Prelude is still in scope after loadNonsense
        fails (eval "2 + 2 :: Int") @@? "Prelude should not be in path"
        
        -- but we do expect the interpreter state to be in a good enough state
        -- to evaluate builtins
        (eval "[1..4]"  @@?= "[1,2,3,4]")

        -- bring the prelude back into scope for the rest of the tests
        setImports ["Prelude"]

        -- make sure we catch exceptions in setTopLevelModules, and that the
        -- interpreter is still in a good state afterwards
        (setTopLevelNonsense1 `catch` handleNotAllowed) @@?= "caught"
        eval "2 + 2 :: Int"  @@?= "4"
        (setTopLevelNonsense2 `catch` handleNotAllowed) @@?= "caught"
        eval "2 + 2 :: Int"  @@?= "4"
    where detonate explosiveThunk = return $! explosiveThunk
          importNonsense = do
            setImports ["NonExistentModule"]
            return "imported a non-existent module"
          loadNonsense = do
            loadModules ["NonExistentFile.hs"]
            return "loaded a non-existent file"
          setTopLevelNonsense1 = do
            setTopLevelModules ["Prelude"]
            return "looking inside the Prelude's internals"
          setTopLevelNonsense2 = do
            setTopLevelModules ["NonLoadedModule"]
            return "looking inside a module which wasn't loaded"
          handleDivideByZero DivideByZero = return "caught"
          handleDivideByZero e = throwM e
          handleWontCompile (WontCompile _) = return "caught"
          handleWontCompile e = throwM e
          handleNotAllowed (NotAllowed _) = return "caught"
          handleNotAllowed e = throwM e

#ifndef THREAD_SAFE_LINKER
-- Prior to ghc-8.10, the linker wasn't thread-safe, and so running multiple
-- instances of hint in different threads can lead to mysterious errors of the
-- form "Could not load '*_closure', dependency unresolved". To make that error
-- less mysterious, 'ifInterpreterNotRunning' throw a clearer error earlier, as
-- soon as it detects that the user is trying to run multiple instances of hint
-- in parallel. This test ensures that this nicer error is thrown.
test_only_one_instance :: TestCase
test_only_one_instance = TestCase "only_one_instance" [] $ liftIO $ do
        r <- newEmptyMVar
        let concurrent = runInterpreter (liftIO $ putMVar r False)
                          `catch` \MultipleInstancesNotAllowed ->
                                    do liftIO $ putMVar r True
                                       return $ Right ()
        _ <- forkIO $ Control.Monad.void concurrent
        readMVar r @?  "concurrent instance did not fail"
#else
-- Prior to ghc-8.10, the linker wasn't thread-safe, and so running multiple
-- instances of hint in different threads can lead to mysterious errors of the
-- form "Could not load '*_closure', dependency unresolved". This test ensures
-- that this error no longer occurs. The important thing about this test is
-- that it should _fail_ if 'ifInterpreterNotRunning' is deleted and this test
-- is run on an older ghc version. Otherwise this test is not testing what it's
-- meant to.
test_multiple_instances :: TestCase
test_multiple_instances = TestCase "multiple_instances" [mod_file] $ liftIO $ do
        writeFile mod_file "f = id"

        -- ensure the two threads interleave in a deterministic way
        tvar <- newTVarIO 1
        let step n = liftIO $ atomically $ do
              nextStep <- readTVar tvar
              guard (nextStep >= n)
              writeTVar tvar (n + 1)
            skipToStep n = liftIO $ atomically $ do
              modifyTVar tvar (max n)

        mvar1 <- newEmptyMVar
        mvar2 <- newEmptyMVar
        void $ forkIO $ do
          r1 <- try $ runInterpreter $ do
            step 1
            loadModules [mod_file]
            step 3
            setTopLevelModules ["Main"]
            step 5
            setImports ["Prelude"]
            step 7
            eval "f [1,2]" @@?= "[1,2]"
            step 9
          skipToStep 9
          putMVar mvar1 r1
        void $ forkIO $ do
          r2 <- try $ runInterpreter $ do
            step 2
            loadModules [mod_file]
            step 4
            setTopLevelModules ["Main"]
            step 6
            setImports ["Prelude"]
            step 8
            eval "f [1,2]" @@?= "[1,2]"
            step 10
          skipToStep 10
          putMVar mvar2 r2
        noInterpreterError =<< noExceptions =<< takeMVar mvar1
        noInterpreterError =<< noExceptions =<< takeMVar mvar2
    where mod_file = "TEST_MultipleInstances.hs"
#endif

test_normalize_type :: TestCase
test_normalize_type = TestCase "normalize_type" [mod_file] $ do
        liftIO $ writeFile mod_file mod_text
        loadModules [mod_file]
        setTopLevelModules ["T"]
        normalizeType "Foo Int" @@?= "()"

    where mod_text = unlines ["{-# LANGUAGE TypeFamilies #-}"
                             ,"module T where"
                             ,"type family Foo x"
                             ,"type instance Foo x = ()"]
          mod_file = "TEST_NormalizeType.hs"

test_package_db :: IOTestCase
test_package_db = IOTestCase "package_db" [dir] $ \wrapInterp -> do
        setup
        ghcVersionOutput <- readProcessStdout_ $ proc "ghc" ["--version"]
        let ghcVersion
              :: String
            ghcVersion
              = ghcVersionOutput
                -- "The Glorious Glasgow Haskell Compilation System, version 8.8.4" :: ByteString.Lazy
              & ByteString.toStrict
                -- "The Glorious Glasgow Haskell Compilation System, version 8.8.4" :: ByteString
              & Text.decodeUtf8
                -- "The Glorious Glasgow Haskell Compilation System, version 8.8.4" :: Text
              & Text.unpack
                -- "The Glorious Glasgow Haskell Compilation System, version 8.8.4" :: String
              & words
                -- ["The","Glorious","Glasgow","Haskell","Compilation","System,","version","8.8.4"]
              & last
                -- "8.8.4"
        let pkgdb    = dir </> "dist-newstyle" </> "packagedb" </> ("ghc-" ++ ghcVersion)
            ghc_args = ["-package-db=" ++ pkgdb,"-package-env","-"]

        -- stack sets GHC_ENVIRONMENT to a file which pins down the versions of
        -- all the packages we can load, and since it does not list my-package,
        -- we cannot load it.
        unsetEnv "GHC_ENVIRONMENT"

        wrapInterp (unsafeRunInterpreterWithArgs ghc_args) $ do
          succeeds (setImports [mod]) @@? "module from package-db must be visible"
    --
    where pkg      = "my-package"
          dir      = pkg
          mod_file = dir </> mod <.> "hs"
          mod      = "MyModule"
          cabal_file = dir </> pkg <.> "cabal"
          setup    = do createDirectory dir
                        writeFile cabal_file $ unlines
                          [ "cabal-version: 2.4"
                          , "name: " ++ pkg
                          , "version: 0.1.0.0"
                          , ""
                          , "library"
                          , "  exposed-modules: " ++ mod
                          ]
                        writeFile mod_file $ unlines
                          [ "{-# LANGUAGE NoImplicitPrelude #-}"
                          , "module " ++ mod ++ " where"
                          ]
                        env <- getEnvironment
                        runProcess_
                          $ setWorkingDir dir
                          $ -- stack sets GHC_PACKAGE_PATH, but cabal complains
                            -- that it cannot run if that variable is set.
                            setEnv (filter ((/= "GHC_PACKAGE_PATH") . fst) env)
                          $ proc "cabal" ["build"]

test_ghc_environment_file :: IOTestCase
test_ghc_environment_file = IOTestCase "ghc_environment_file" [dir] $ \wrapInterp -> do
        setup

        -- stack sets GHC_ENVIRONMENT to a file which pins down the versions of
        -- all the packages we can load, and since it does not list my-package,
        -- we cannot load it.
        unsetEnv "GHC_ENVIRONMENT"

        wrapInterp runInterpreter $ do
          fails (setImports ["Acme.Dont"]) @@? "acme-dont should not be in path"

        -- in dir, there is a file .ghc.environment.<something> which lists the
        -- acme-dont package because it is a dependency of my-package. hint
        -- should detect that file and make containers available to the
        -- interpreted code.
        withCurrentDirectory dir $ do
          wrapInterp runInterpreter $ do
            succeeds (setImports ["Acme.Dont"]) @@? "acme-dont should be in path"
    --
    where pkg      = "my-package"
          dir      = pkg
          mod_file = dir </> mod <.> "hs"
          mod      = "MyModule"
          cabal_file = dir </> pkg <.> "cabal"
          setup    = do createDirectory dir
                        writeFile cabal_file $ unlines
                          [ "cabal-version: 2.4"
                          , "name: " ++ pkg
                          , "version: 0.1.0.0"
                          , ""
                          , "library"
                          , "  exposed-modules: " ++ mod
                          , "  build-depends: acme-dont"
                          ]
                        writeFile mod_file $ unlines
                          [ "{-# LANGUAGE NoImplicitPrelude #-}"
                          , "module " ++ mod ++ " where"
                          ]
                        env <- getEnvironment
                        runProcess_
                          $ proc "cabal" ["update"]
                        runProcess_
                          $ setWorkingDir dir
                          $ -- stack sets GHC_PACKAGE_PATH, but cabal complains
                            -- that it cannot run if that variable is set.
                            setEnv (filter ((/= "GHC_PACKAGE_PATH") . fst) env)
                          $ proc "cabal" ["build", "--write-ghc-environment-files=always"]

-- earlier versions of hint were accidentally overwriting the signal handlers
-- for ^C and others.
--
-- note that hint was _not_ overwriting the signal handlers when the hint interpreter
-- was itself executed inside the ghci interpreter. for this reason, this test always
-- succeeds when executed from ghci and ghcid, regardless of whether the problematic
-- behaviour has been fixed or not.
test_signal_handlers :: IOTestCase
test_signal_handlers = IOTestCase "signal_handlers" [] $ \wrapInterp -> do
#if defined(mingw32_HOST_OS) || defined(__MINGW32__)
        wrapInterp runInterpreter $ do
          pure ()
#else
        signalDetectedRef <- newIORef False
        interruptDetectedRef <- newIORef False
        let detectSignal = writeIORef signalDetectedRef True
            detectInterrupt = writeIORef interruptDetectedRef True
            acquire = installHandler sigINT (Catch detectSignal) Nothing
            release handler = installHandler sigINT handler Nothing
        r <- bracket acquire release $ \_ -> do
          wrapInterp runInterpreter $ do
            liftIO $ do
              r <- try $ do
                raiseSignal sigINT
                threadDelay 1000000  -- will be interrupted by the above signal
              case r of
                Left UserInterrupt -> do
                  -- hint is _still_ accidentally overwriting the signal handler :(
                  detectInterrupt
                Left e -> do
                  -- some other async exception, rethrow
                  throwM e
                Right () ->
                  return ()
        signalDetected <- readIORef signalDetectedRef
        signalDetected @?= True
        interruptDetected <- readIORef interruptDetectedRef
        interruptDetected @?= False
        return r
#endif

test_error_capture :: IOTestCase
test_error_capture = IOTestCase "error_capture" [mod_file] $ \wrapInterp-> do
        liftIO $ writeFile mod_file "$"
        r <- wrapInterp runInterpreter $ do
          loadModules [mod_file]
        case r of
          Right () -> assertFailure "Loaded invalid file"
          Left (WontCompile _) -> pure $ Right ()
          Left e -> assertFailure $ "Got other than WontCompiler error: " ++ show e

    where mod_name = "TEST_ErrorCapture"
          mod_file = mod_name ++ ".hs"

tests :: [TestCase]
tests = [test_reload_modified
        ,test_lang_exts
        ,test_work_in_main
        ,test_comments_in_expr
        ,test_qual_import
        ,test_full_import
        ,test_basic_eval
        ,test_eval_layout
        ,test_show_in_scope
        ,test_installed_not_in_scope
        ,test_priv_syms_in_scope
        ,test_search_path
        ,test_search_path_dot
        ,test_catch
#ifndef THREAD_SAFE_LINKER
        ,test_only_one_instance
#else
        ,test_multiple_instances
#endif
        ,test_normalize_type
        ]

ioTests :: [IOTestCase]
ioTests = [test_signal_handlers
          ,test_package_db
          ,test_ghc_environment_file
          ,test_error_capture
          ]

main :: IO ()
main = do -- run the tests...
          c1 <- runTests False tests
          c2 <- runIOTests False ioTests
          -- then run again, but with sandboxing on...
          c3 <- runTests True tests
          c4 <- runIOTests True ioTests
          --
          let failures  = HUnit.errors c1 + HUnit.failures c1 +
                          HUnit.errors c2 + HUnit.failures c2 +
                          HUnit.errors c3 + HUnit.failures c3 +
                          HUnit.errors c4 + HUnit.failures c4
              exit_code
                  | failures > 0 = ExitFailure failures
                  | otherwise    = ExitSuccess
          exitWith exit_code
       -- `catch` (\_ -> exitWith (ExitFailure $ -1))

printInterpreterError :: InterpreterError -> IO ()
printInterpreterError = hPrint stderr

setSandbox :: Interpreter ()
setSandbox = set [installedModulesInScope := False]

(@@?) :: (HUnit.AssertionPredicable p, MonadIO m) => m p -> String -> m ()
p @@? msg = do b <- p; liftIO (b @? msg)

(@@?=) :: (Eq a, Show a, MonadIO m) => m a -> a -> m ()
m_a @@?= b = do a <- m_a; liftIO (a @?= b)

fails :: (MonadCatch m, MonadIO m) => m a -> m Bool
fails action = (action >> return False) `catchIE` (\_ -> return True)
  where
    catchIE :: MonadCatch m => m a -> (InterpreterError -> m a) -> m a
    catchIE = MC.catch

succeeds :: (MonadCatch m, MonadIO m) => m a -> m Bool
succeeds = fmap not . fails

noExceptions :: Either SomeException a -> IO a
noExceptions (Left  e) = assertFailure (show e)
noExceptions (Right a) = pure a

noInterpreterError :: Either InterpreterError a -> IO a
noInterpreterError (Left  e) = assertFailure (show e)
noInterpreterError (Right a) = pure a

data IOTestCase = IOTestCase
  String  -- test name
  [FilePath]  -- temporary files and folders to delete after the test
  ( ( (Interpreter () -> IO (Either InterpreterError ()))
   -> (Interpreter () -> IO (Either InterpreterError ()))
    ) -- please wrap your 'runInterpreter' calls with this
 -> IO (Either InterpreterError ())  -- create temporary files and run the test
  )

runIOTests :: Bool -> [IOTestCase] -> IO HUnit.Counts
runIOTests sandboxed = HUnit.runTestTT . HUnit.TestList . map build
    where build (IOTestCase title tmps test)
            = HUnit.TestLabel title $ HUnit.TestCase test_case
            where test_case = go `finally` clean_up
                  clean_up = mapM_ removeIfExists tmps
                  wrapInterp runInterp body = runInterp (when sandboxed setSandbox >> body)
                  go       = do r <- test wrapInterp
                                noInterpreterError r
                  removeIfExists f = do existsF <- doesFileExist f
                                        if existsF
                                          then removeFile f
                                          else
                                            do existsD <- doesDirectoryExist f
                                               when existsD $
                                                  removeDirectoryRecursive f

data TestCase = TestCase
  String  -- test name
  [FilePath]  -- temporary files and folders to delete after the test
  (Interpreter ())  -- create temporary files and run the test

runTests :: Bool -> [TestCase] -> IO HUnit.Counts
runTests sandboxed = runIOTests sandboxed . map toIOTestCase
  where
    toIOTestCase :: TestCase -> IOTestCase
    toIOTestCase (TestCase title tmps test) = IOTestCase title tmps $ \wrapInterp -> do
      wrapInterp runInterpreter test
