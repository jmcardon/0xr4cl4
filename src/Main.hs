{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell     #-}

module Main where

import           Data.Text                      ( Text )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as TIO
import qualified Data.List                     as L
import           Data.List.NonEmpty             ( NonEmpty(..) )
import qualified System.Directory              as Dir
import qualified System.Process                as Proc
import           System.FilePath.Posix          ( (</>) )
import qualified System.FilePath.Posix         as FP
import           Control.Lens                   ( makeLenses
                                                , (^.)
                                                )
import           Control.Monad                  ( filterM )
import           Data.Maybe                     ( catMaybes )
import           System.Exit                    ( ExitCode(..) )
import           Control.Exception              ( Exception
                                                , throwIO
                                                )
import           Options.Applicative            ( Parser
                                                , ParserInfo
                                                , strOption
                                                , long
                                                , help
                                                , metavar
                                                , action
                                                , info
                                                , fullDesc
                                                , progDesc
                                                , (<**>)
                                                , header
                                                , helper
                                                , execParser
                                                )
import           Data.Foldable                  ( traverse_ )
import           Data.Algorithm.Diff
import           System.Console.ANSI

data ProcessException = OracleFailure Int String String | EiffelFailure Int String String

instance Show ProcessException where
    show (OracleFailure i file errorOut) = "Oracle program failure running file "
      <> file
      <> " with exit code "
      <> show i
      <> " with error output: \n"
      <> errorOut
    show (EiffelFailure i file errorOut) = "User program failure running file "
      <> file
      <> " with exit code "
      <> show i
      <> " with error output: \n"
      <> errorOut

instance Exception ProcessException

data Env = Env {
    _envProgramPath :: FilePath,
    _envOraclePath:: FilePath,
    _envFolderPath :: FilePath
} deriving (Eq, Show)

makeLenses ''Env

data TextLine = TextLine {
    _textLineNum :: Int,
    _textLineContent :: Text
} deriving (Eq, Show)

makeLenses ''TextLine

data FileDiffs = FileDiffs {
    _fdiffFile :: FilePath,
    _fdiffs :: NonEmpty (TextLine, TextLine)
} deriving (Eq, Show)

makeLenses ''FileDiffs

main :: IO ()
main = do
  env <- execParser cmdOptsEnv
  eiffelFile env

cmdOptsEnv :: ParserInfo Env
cmdOptsEnv = info
  (parseEnv <**> helper)
  (  fullDesc
  <> progDesc "Compare the program output against the provided eiffel oracle"
  <> header "Oracle diff util"
  )

parseEnv :: Parser Env
parseEnv =
  Env
    <$> strOption
          (  long "eiffel-path"
          <> help "the eiffel project executable file path (not oracle)"
          <> metavar "FILE"
          <> action "file"
          )
    <*> strOption
          (  long "oracle-path"
          <> help "the oracle file path"
          <> metavar "FILE"
          <> action "file"
          )
    <*> strOption
          (  long "at-file-dir"
          <> help "the \"at\" test files directory"
          <> metavar "DIR"
          <> action "directory"
          )

lineDiffs :: FilePath -> [(TextLine, TextLine)] -> Maybe FileDiffs
lineDiffs f li = case filter eqTextContent li of
  []     -> Nothing
  x : xs -> Just $ FileDiffs f (x :| xs)
  where eqTextContent (l, r) = (l ^. textLineContent) /= (r ^. textLineContent)

allFiles :: [FilePath] -> [FilePath]
allFiles = filter atFiles
 where
  atFiles fp =
    L.isPrefixOf "at" fp
      && (FP.takeExtension fp == ".txt")
      && (not $ L.isInfixOf "expected" fp)
      && (not $ L.isInfixOf "actual" fp)

eiffelFile :: Env -> IO () -- output
eiffelFile env = do
  let folder = (env ^. envFolderPath)
  rawDirs <- Dir.listDirectory folder -- filterM Dir.doesFileExist
  let filtered  = allFiles rawDirs
      combPath  = (</>) folder <$> filtered
      niceFiles = ("-- " ++) <$> filtered
  putStrLn "Program diffs compared for the following files:"
  traverse_ putStrLn niceFiles
  allF <- filterM Dir.doesFileExist combPath
  li   <- catMaybes <$> traverse (runAll env) allF
  case li of
    [] -> niceOutput "No differences in all files run! Good job"
    _  -> traverse_ diffs li
 where
  niceOutput e = do
    setSGR [SetColor Foreground Vivid Blue]
    putStrLn e
    setSGR [Reset]
  runAll env fp = do
    (eOracle, outOracle, errOracle) <- Proc.readProcessWithExitCode
      (env ^. envOraclePath)
      ["-b", fp]
      []
    handleExit eOracle errOracle fp OracleFailure
    (eEiffel, outEiffel, errEiffel) <- Proc.readProcessWithExitCode
      (env ^. envProgramPath)
      ["-b", fp]
      []
    handleExit eEiffel errEiffel fp EiffelFailure
    pure
      $     lineDiffs fp
      $     mkTextLines ((T.lines . T.pack) outOracle)
      `zip` mkTextLines ((T.lines . T.pack) outEiffel)
  mkTextLines li = uncurry TextLine <$> ([1 ..] `zip` li)
  handleExit (ExitFailure i) err file f = throwIO $ f i file err
  handleExit _               _   _    _ = pure ()
  diffs fd = do
    setSGR [SetColor Foreground Dull Red]
    TIO.putStrLn
      $  "Found differences after running "
      <> (T.pack $ fd ^. fdiffFile)
      <> " (difference in red)"
    setSGR [Reset]
    traverse_ diffPrint (fd ^. fdiffs)
  diffPrint (o, e) = do
    putStrLn "Oracle:"
    putStr $ "  Line " <> show (o ^. textLineNum) <> ": "
    TIO.putStrLn $ o ^. textLineContent
    putStrLn "Eiffel:"
    putStr $ "  Line " <> show (o ^. textLineNum) <> ": "
    traverse_ ansiPrintDiffs $ getGroupedDiff
      (T.unpack $ o ^. textLineContent)
      (T.unpack $ e ^. textLineContent)
    putStrLn "\n"
  ansiPrintDiffs (Both a _) = putStr a
  ansiPrintDiffs (Second a) = do
    setSGR [SetColor Foreground Vivid Red]
    putStr a
    setSGR [Reset]
  ansiPrintDiffs _ = pure ()