module Main (main) where

import Prelude hiding (catch)

import Control.Exception.Extensible (ArithException(..))
import Control.Monad.Catch as MC

import Control.Monad (liftM, when, void, (>=>))

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar

import Data.Proxy

import System.IO
import System.FilePath
import System.Directory
import System.Exit

import Test.HUnit ((@?=), (@?))
import qualified Test.HUnit as HUnit

import Language.Haskell.Interpreter

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
                        interpret "f" (Proxy :: Proxy (Int -> Int))

test_lang_exts :: TestCase
test_lang_exts = TestCase "lang_exts" [mod_file] $ do
                      liftIO $ writeFile mod_file "data T where T :: T"
                      fails do_load @@? "first time, it shouldn't load"
                      --
                      set [languageExtensions := [GADTs]]
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
                        interpret "f $ 1 == 2" Proxy @@?= False
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
                            _ <- interpret expr (Proxy :: Proxy Int)
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
        succeeds (action `catch` handler) @@? "catch failed"
    where handler DivideByZero = return "catched"
          handler e = throwM e
          action = do s <- eval "1 `div` 0 :: Int"
                      return $! s

test_only_one_instance :: TestCase
test_only_one_instance = TestCase "only_one_instance" [] $ liftIO $ do
        r <- newEmptyMVar
        let concurrent = runInterpreter (liftIO $ putMVar r False)
                          `catch` \MultipleInstancesNotAllowed ->
                                    do liftIO $ putMVar r True
                                       return $ Right ()
        _ <- forkIO $ Control.Monad.void concurrent
        readMVar r @?  "concurrent instance did not fail"

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
        ,test_only_one_instance
        ,test_normalize_type
        ]

main :: IO ()
main = do -- run the tests...
          c  <- runTests False tests
          -- then run again, but with sandboxing on...
          c' <- runTests True tests
          --
          let failures  = HUnit.errors c  + HUnit.failures c +
                          HUnit.errors c' + HUnit.failures c'
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
succeeds = liftM not . fails

data TestCase = TestCase String [FilePath] (Interpreter ())

runTests :: Bool -> [TestCase] -> IO HUnit.Counts
runTests sandboxed = HUnit.runTestTT . HUnit.TestList . map build
    where build (TestCase title tmps test) = HUnit.TestLabel title $
                                                 HUnit.TestCase test_case
            where test_case = go `finally` clean_up
                  clean_up = mapM_ removeIfExists tmps
                  go       = do r <- runInterpreter
                                            (when sandboxed setSandbox >> test)
                                either (printInterpreterError >=> (fail . show))
                                       return r
                  removeIfExists f = do existsF <- doesFileExist f
                                        if existsF
                                          then removeFile f
                                          else
                                            do existsD <- doesDirectoryExist f
                                               when existsD $
                                                  removeDirectory f
