import System.Environment
import System.Console.GetOpt

data Options = Options { optOutput :: Maybe FilePath
                       , optExpr :: Maybe String
                       , optAssembly :: Bool
                       } deriving Show

defaultOptions = Options { optOutput = Nothing
						 , optExpr = Nothing
					     , optAssembly = False
                         }

argSpec = [ Option ['o'] ["output"]
				(ReqArg (\d opts -> opts { optOutput = Just d }) "FILE")
				"output file"
		  , Option ['e'] ["expr"]
		  		(ReqArg (\e opts -> opts { optExpr = Just e }) "EXPRESSION")
		  		"expression"
		  , Option ['s'] ["assembly"]
		  		(NoArg (\opts -> opts { optAssembly = True }))
		  		"assembly code"
		  ]


parseArgs spec argv
	= case getOpt Permute spec argv of
        (o,n,[]  ) -> (foldl (flip id) defaultOptions o, n)
        (_,_,errs) -> error (concat errs ++ usageInfo usage argSpec)

usage = "Usage: XXXX [options] [file]"

main = do
	progName <- getProgName
	putStrLn progName
	args <- getArgs
	print $ parseArgs argSpec args
