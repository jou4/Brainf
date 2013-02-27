{-# OPTIONS_GHC -cpp #-}

module Main where

import System.Environment (getArgs)
import System.IO
import Data.Char (chr, ord)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.State
import Text.Printf (printf, hPrintf)

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


data EmitState = EmitState { getHandle :: Handle, getLoopStack :: [Int], getPtrLoaded :: Bool }

#if defined(mingw32_HOST_OS)
-- Windows
#elif defined(darwin_HOST_OS)
-- MacOSX
labelPrefix = "_"
syscallRead = "0x2000003"
syscallWrite = "0x2000004"
#else
-- Linux
labelPrefix = ""
syscallRead = "3"
syscallWrite = "4"
#endif

label name = labelPrefix ++ name
labelMain = label "main"
labelPutc = label "putc"
labelGetc = label "getc"
labelLoopBegin level = label $ "loop.begin." ++ (show level)
labelLoopEnd level = label $ "loop.end." ++ (show level)
sizeOfFrame = 30008 :: Int

asm :: FilePath -> [Inst] -> IO ()
asm filePath insts = withFile filePath WriteMode $ \h -> do
  hPutStr h "  .text\n"
  hPrintf h "%s:\n" labelPutc
  hPutStr h "  pushq %rbp\n"
  hPutStr h "  movq %rsp, %rbp\n"
  hPutStr h "  movl %edi, -4(%rbp)\n"
  hPrintf h "  movq $%s, %%rax\n" syscallWrite
  hPutStr h "  movq $1, %rdi\n"
  hPutStr h "  leaq -4(%rbp), %rsi\n"
  hPutStr h "  movq $1, %rdx\n"
  hPutStr h "  syscall\n"
  hPutStr h "  leave\n"
  hPutStr h "  ret\n"
  hPrintf h "%s:\n" labelGetc
  hPutStr h "  pushq %rbp\n"
  hPutStr h "  movq %rsp, %rbp\n"
  hPutStr h "  movl %edi, -4(%rbp)\n"
  hPrintf h "  movq $%s, %%rax\n" syscallRead
  hPutStr h "  movq $1, %rdi\n"
  hPutStr h "  leaq -4(%rbp), %rsi\n"
  hPutStr h "  movq $1, %rdx \n"
  hPutStr h "  syscall\n"
  hPutStr h "  movl -4(%rbp), %eax\n"
  hPutStr h "  leave\n"
  hPutStr h "  ret\n"
  hPrintf h ".globl %s\n" labelMain
  hPrintf h "%s:\n" labelMain
  hPutStr h "  pushq %rbp\n"
  hPutStr h "  movq %rsp, %rbp\n"
  hPrintf h "  subq $%d, %%rsp\n" sizeOfFrame
  hPutStr h "  movq $0, -8(%rbp)\n"
  evalStateT (emit insts) (EmitState h [0] False)
  hPutStr h "  movb $0x0a, %dil\n"
  hPrintf h "  call %s\n" labelPutc
  hPutStr h "  leave\n"
  hPutStr h "  ret\n"

write :: String -> StateT EmitState IO ()
write s = writeLabel ("  " ++ s)

writeLabel :: String -> StateT EmitState IO ()
writeLabel s = do h <- gets getHandle
                  liftIO $ hPutStrLn h s

ptrLoaded :: EmitState -> EmitState
ptrLoaded (EmitState h s _) = EmitState h s True

ptrUnloaded :: EmitState -> EmitState
ptrUnloaded (EmitState h s _) = EmitState h s False

loadPtr :: StateT EmitState IO ()
loadPtr = do loadNeeded <- gets (not . getPtrLoaded)
             when loadNeeded $ write "movq -8(%rbp), %rsi"
             modify ptrLoaded

genLoopLevel :: StateT EmitState IO Int
genLoopLevel = do EmitState h stack l <- get
                  let level = head stack + 1
                  put $ EmitState h (level:stack) l
                  return level

readLoopLevel :: StateT EmitState IO Int
readLoopLevel = gets $ head . getLoopStack

pushLoopLevel :: Int -> EmitState -> EmitState
pushLoopLevel level (EmitState h stack l) = EmitState h (level:stack) l

popLoopLevel :: EmitState -> EmitState
popLoopLevel (EmitState h [] l) = EmitState h [] l
popLoopLevel (EmitState h (x:xs) l) = EmitState h xs l

emit :: [Inst] -> StateT EmitState IO ()
emit [] = return ()
emit (IncPtr:insts) = write "incq -8(%rbp)" >> modify ptrUnloaded >> emit insts
emit (DecPtr:insts) = write "decq -8(%rbp)" >> modify ptrUnloaded >> emit insts
emit (IncVal:insts) = loadPtr >> write "incb (%rsp,%rsi,1)" >> emit insts
emit (DecVal:insts) = loadPtr >> write "decb (%rsp,%rsi,1)" >> emit insts
emit (PutVal:insts) = do loadPtr
                         write "movb (%rsp,%rsi,1), %dil"
                         write $ "call " ++ labelPutc
                         modify ptrUnloaded
                         emit insts
emit (GetVal:insts) = do write $ "call " ++ labelGetc
                         modify ptrUnloaded
                         loadPtr
                         write "movb %al, (%rsp,%rsi,1)"
                         emit insts
emit (Block block:insts) = do
  loopLevel <- genLoopLevel
  loadPtr
  write $ "movb (%rsp,%rsi,1), %dil"
  write $ "cmpb $0, %dil"
  write $ "je " ++ labelLoopEnd loopLevel
  writeLabel $ labelLoopBegin loopLevel ++ ":"
  modify ptrUnloaded
  emit block
  emit insts
emit (Recur:insts) = do
  loopLevel <- readLoopLevel
  loadPtr
  write $ "movb (%rsp,%rsi,1), %dil"
  write $ "cmpb $0, %dil"
  write $ "jne " ++ labelLoopBegin loopLevel
  writeLabel $ labelLoopEnd loopLevel ++ ":"
  modify $ ptrUnloaded . popLoopLevel
  emit insts

