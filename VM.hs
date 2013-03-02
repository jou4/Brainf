module VM where

import System.IO
import Data.Char (chr, ord)
import Control.Applicative ((<$>), (<*>))
import Control.Monad.State
import Compiler

interpret :: [Char] -> IO ()
interpret code = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  runStateT (eval (compile code)) $ EvalState { getPtr = 0, getVals = repeat 0 }
  putStrLn ""

type Ptr = Int
type Val = Int
data EvalState = EvalState
    { getPtr :: Ptr
    , getVals :: [Val]
    }

modPtr :: (Ptr -> Ptr) -> EvalState -> EvalState
modPtr f st = st { getPtr = f $ getPtr st }

incPtr :: EvalState -> EvalState
incPtr = modPtr (+1)

decPtr :: EvalState -> EvalState
decPtr = modPtr $ \p -> p - 1

getVal :: EvalState -> Val
getVal (EvalState p vs) = vs !! p

putVal :: EvalState -> Val -> EvalState
putVal st@(EvalState p vs) v = st { getVals = replaceNth p v vs }

replaceNth :: Int -> b -> [b] -> [b]
replaceNth n v [] = []
replaceNth n v (x:xs) = if n == 0 then v : xs else x : replaceNth (n - 1) v xs

incVal :: EvalState -> EvalState
incVal st@(EvalState p vs) = putVal st $ getVal st + 1

decVal :: EvalState -> EvalState
decVal st@(EvalState p vs) = putVal st $ getVal st - 1

eval :: [Inst] -> StateT EvalState IO Bool
eval [] = return False
eval (IncPtr:insts) = modify incPtr >> eval insts
eval (DecPtr:insts) = modify decPtr >> eval insts
eval (IncVal:insts) = modify incVal >> eval insts
eval (DecVal:insts) = modify decVal >> eval insts
eval (PutVal:insts) = do st <- get
                         liftIO $ putChar $ chr $ getVal st
                         eval insts
eval (GetVal:insts) = do val <- liftIO $ inputPrompt
                         modify $ (flip putVal) val
                         eval insts
  where inputPrompt = do putStr "Input char: "
                         val <- ord <$> getChar
                         putStrLn ""
                         return val
eval loop@(Block block:insts) = do recur <- eval block
                                   if recur then eval loop else eval insts
eval (Recur:insts) = do v <- getVal <$> get
                        return $ v /= 0

