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
  runStateT (eval (compile code)) (0, repeat 0)
  putStrLn ""

type Ptr = Int
type Val = Int
type BFState = (Ptr, [Val])

modPtr :: (Ptr -> Ptr) -> BFState -> BFState
modPtr f (p,vs) = (f p, vs)

incPtr :: BFState -> BFState
incPtr = modPtr (+1)

decPtr :: BFState -> BFState
decPtr = modPtr $ \p -> p - 1

getVal :: BFState -> Val
getVal (p,vs) = vs !! p

putVal :: BFState -> Val -> BFState
putVal (p,vs) v = (p, replaceNth p v vs)

replaceNth :: Int -> b -> [b] -> [b]
replaceNth n v [] = []
replaceNth n v (x:xs) = if n == 0 then v : xs else x : replaceNth (n - 1) v xs

incVal :: BFState -> BFState
incVal st@(p,vs) = putVal st $ getVal st + 1

decVal :: BFState -> BFState
decVal st@(p,vs) = putVal st $ getVal st - 1

eval :: [Inst] -> StateT BFState IO Bool
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

