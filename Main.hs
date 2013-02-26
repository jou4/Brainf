module Main where

import System.Environment (getArgs)
import System.IO
import Data.Char (chr, ord)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State

hello = "+++++++++[>++++++++>+++++++++++>+++++<<<-]>.>++.+++++++..+++.>-.------------.<++++++++.--------.+++.------.--------.>+."
inputABC = ",>,>,.<.<."

main :: IO ()
main = do
  args <- getArgs
  when (length args > 0) $ interpret $ args !! 0

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


data EmitState = EmitState { getHandle :: Handle, getFrameStack :: [String], getPtrLoaded :: Bool }

asm :: FilePath -> [Inst] -> IO ()
asm filePath insts = withFile filePath WriteMode $ \h -> do
  hPutStrLn h "  .text"
  hPutStrLn h "_putc:"
  hPutStrLn h "  pushq %rbp"
  hPutStrLn h "  movq %rsp, %rbp"
  hPutStrLn h "  movl %edi, -4(%rbp)"
  hPutStrLn h "  movq $0x2000004, %rax"
  hPutStrLn h "  movq $1, %rdi"
  hPutStrLn h "  leaq -4(%rbp), %rsi"
  hPutStrLn h "  movq $1, %rdx"
  hPutStrLn h "  syscall"
  hPutStrLn h "  leave"
  hPutStrLn h "  ret"
  hPutStrLn h "_getc:"
  hPutStrLn h "  pushq %rbp"
  hPutStrLn h "  movq %rsp, %rbp"
  hPutStrLn h "  movl %edi, -4(%rbp)"
  hPutStrLn h "  movq $0x2000003, %rax"
  hPutStrLn h "  movq $1, %rdi"
  hPutStrLn h "  leaq -4(%rbp), %rsi"
  hPutStrLn h "  movq $1, %rdx "
  hPutStrLn h "  syscall"
  hPutStrLn h "  movl -4(%rbp), %eax"
  hPutStrLn h "  leave"
  hPutStrLn h "  ret"
  hPutStrLn h ".globl _main"
  hPutStrLn h "_main:"
  hPutStrLn h "  pushq %rbp"
  hPutStrLn h "  movq %rsp, %rbp"
  hPutStrLn h "  subq $30008, %rsp"
  evalStateT (emit insts) (EmitState h [] False)
  hPutStrLn h "  leave"
  hPutStrLn h "  ret"

write :: String -> StateT EmitState IO ()
write s = do h <- gets getHandle
             liftIO $ hPutStrLn h ("  " ++ s)

ptrLoaded :: EmitState -> EmitState
ptrLoaded (EmitState h fs _) = EmitState h fs True

ptrUnloaded :: EmitState -> EmitState
ptrUnloaded (EmitState h fs _) = EmitState h fs False

loadPtr :: StateT EmitState IO ()
loadPtr = do loadNeeded <- gets (not . getPtrLoaded)
             when loadNeeded $ write "movq -8(%rbp), %rsi"
             modify ptrLoaded

emit :: [Inst] -> StateT EmitState IO ()
emit [] = return ()
emit (IncPtr:insts) = write "incq -8(%rbp)" >> modify ptrUnloaded >> emit insts
emit (DecPtr:insts) = write "decq -8(%rbp)" >> modify ptrUnloaded >> emit insts
emit (IncVal:insts) = loadPtr >> write "incb (%rsp,%rsi,1)" >> emit insts
emit (DecVal:insts) = loadPtr >> write "decb (%rsp,%rsi,1)" >> emit insts
emit (PutVal:insts) = do loadPtr
                         write "movb (%rsp,%rsi,1), %dil"
                         write "call _putc"
                         modify ptrUnloaded
                         emit insts
emit (GetVal:insts) = do write "call _getc"
                         modify ptrUnloaded
                         loadPtr
                         write "movb %al, (%rsp,%rsi,1)"
                         emit insts
emit (Block block:insts) = undefined
emit (Recur:insts) = undefined

