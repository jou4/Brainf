module Main where

import System.Environment (getArgs, getProgName)
import System.Console.GetOpt
import Compiler
import VM
import Emit

hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."
inputABC = ",>,>,.<.<."

main :: IO ()
main = do
  args <- getArgs
  (opts, remain) <- compilerOpts args
  if (optHelp opts) then do { help <- usage; putStrLn help }
  else do 
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
  , optHelp :: Bool
  } deriving Show

defaultOptions = Options
  { optExpr = Nothing
  , optAssembly = False
  , optOutput = Just "output.s"
  , optHelp = False
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
  , Option ['h'] ["help"]
		  (NoArg (\opts -> opts { optHelp = True }))
		  "show help"
  ]


compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
	case getOpt Permute options argv of
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> do { u <- usage
                     ; error (concat errs ++ u) }

usage :: IO String
usage = do progName <- getProgName
           let header = "Usage: " ++ progName ++ " [Options] [file]"
           return $ usageInfo header options
