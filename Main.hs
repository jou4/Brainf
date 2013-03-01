module Main where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import Control.Monad (when)
import Compiler
import VM
import Emit

hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."
inputABC = ",>,>,.<.<."

main :: IO ()
main = do
  args <- getArgs
  (opts, remain) <- compilerOpts args
  code <- readCode (optExpr opts) remain
  if optAssembly opts then
    case optOutput opts of
      Just output -> writeAssembly output $ compile code
      Nothing -> error "not specified output file."
  else interpret code

readCode :: Maybe String -> [String] -> IO String
readCode (Just e) _ = return e
readCode Nothing [] = getContents
readCode Nothing (f:_) = readFile f

data Options = Options
  { optExpr :: Maybe String
  , optAssembly :: Bool
  , optOutput :: Maybe String
  } deriving Show

defaultOptions = Options
  { optExpr = Nothing
  , optAssembly = False
  , optOutput = Just "output.s"
  }

options :: [OptDescr (Options -> Options)]
options = 
  [ Option ['e'] ["expr"]
		  (ReqArg (\e opts -> opts { optExpr = Just e }) "EXPRESSION")
		  "expression"
  , Option ['s'] ["assembly"]
		  (NoArg (\opts -> opts { optAssembly = True }))
		  "emit assembly code flag"
  , Option ['o'] ["output"]
		  (ReqArg (\e opts -> opts { optOutput = Just e }) "FILE")
		  "output file"
  ]


compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
	case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> do { progName <- getProgName
                     ; error (concat errs ++ usageInfo (header progName) options) }
    where header progName = "Usage: " ++ progName ++ " [Options] [file]"
