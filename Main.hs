module Main where

import System.Environment (getArgs)
import System.IO (stdin, stdout, hSetBuffering, BufferMode(..))
import Data.Char (chr, ord)
import Control.Applicative ((<$>))
import Control.Monad (when)

hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."

main :: IO ()
main = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  args <- getArgs
  when (length args > 0) $ interpret $ args !! 0

interpret code = eval (0, repeat 0) (compile code) >> putStrLn ""

data Inst = IncPtr
          | DecPtr
          | IncVal 
          | DecVal 
          | PutVal 
          | GetVal 
          | Block [Inst] 
          | Recur 
          deriving (Show, Eq)

compile :: [Char] -> [Inst]
compile code = fst . compile' . sanitize $ code
  where sanitize = filter $ flip elem "><+-.,[]"
        compile' [] = ([], [])
        compile' (c:cs) = let (insts, remain) = compile' cs
                          in case c of
                            '>' -> (IncPtr : insts, remain)
                            '<' -> (DecPtr : insts, remain)
                            '+' -> (IncVal : insts, remain)
                            '-' -> (DecVal : insts, remain)
                            '.' -> (PutVal : insts, remain)
                            ',' -> (GetVal : insts, remain)
                            '[' -> let (insts2, remain2) = compile' remain
                                   in (Block insts : insts2, remain2)
                            ']' -> ([Recur], cs)

data S = S { ptr :: Int, vals :: [Int] }

eval :: (Int, [Int]) -> [Inst] -> IO (Bool, (Int, [Int]))
eval st [] = return (False, st)
eval (ptr, vals) (IncPtr:insts) = eval (ptr + 1, vals) insts
eval (ptr, vals) (DecPtr:insts) = eval (ptr - 1, vals) insts
eval (ptr, vals) (IncVal:insts) = eval (ptr, incVal ptr vals) insts
eval (ptr, vals) (DecVal:insts) = eval (ptr, decVal ptr vals) insts
eval (ptr, vals) (PutVal:insts) = do { putChar $ chr (getVal ptr vals); eval (ptr, vals) insts }
eval (ptr, vals) (GetVal:insts) = do 
  putStr "Input char: "
  val <- ord <$> getChar
  putStrLn ""
  eval (ptr, putVal ptr val vals) insts
eval (ptr, vals) loop@(Block block:insts) = do (recur, st2) <- eval (ptr, vals) block
                                               if recur then eval st2 loop else eval st2 insts
eval st@(ptr, vals) (Recur:insts) = return $ (getVal ptr vals /= 0, st)

getVal :: Int -> [Int] -> Int
getVal n vals = vals !! n

putVal :: Int -> Int -> [Int] -> [Int]
putVal n val [] = []
putVal n val (x:xs) = if n == 0 then val : xs else x : putVal (n - 1) val xs

incVal :: Int -> [Int] -> [Int]
incVal n vals = putVal n (getVal n vals + 1) vals

decVal :: Int -> [Int] -> [Int]
decVal n vals = putVal n (getVal n vals - 1) vals
