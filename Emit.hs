{-# OPTIONS_GHC -cpp #-}

module Emit where

import System.IO
import Text.Printf (printf, hPrintf)
import Control.Monad.State
import Compiler

data EmitState = EmitState { getHandle :: Handle, getLoopStack :: [Int], getPtrLoaded :: Bool, getId :: Int }

#if defined(linux_HOST_OS)
-- Linux
labelPrefix = ""
syscallRead = "0"
syscallWrite = "1"
fdStdIn = "0"
fdStdOut = "1"
#elif defined(freebsd_HOST_OS)
-- FreeBSD
labelPrefix = ""
syscallRead = "3"
syscallWrite = "4"
fdStdIn = "0"
fdStdOut = "1"
#elif defined(darwin_HOST_OS)
-- MacOSX
labelPrefix = "_"
syscallRead = "0x2000003"
syscallWrite = "0x2000004"
fdStdIn = "0"
fdStdOut = "1"
#elif defined(mingw32_HOST_OS)
-- Windows TODO
#else
-- Unknown TODO
#endif

label name = labelPrefix ++ name
labelMain = label "main"
labelPutc = label "putc"
labelGetc = label "getc"
labelLoopBegin level = label $ "loop.begin." ++ (show level)
labelLoopEnd level = label $ "loop.end." ++ (show level)
sizeOfFrame = 30008 :: Int

writeAssembly :: FilePath -> [Inst] -> IO ()
writeAssembly filePath insts = withFile filePath WriteMode $ \h -> do
  hPutStr h "  .text\n"
  hPrintf h "%s:\n" labelPutc
  hPutStr h "  pushq %rbp\n"
  hPutStr h "  movq %rsp, %rbp\n"
  hPutStr h "  subq $1, %rsp\n"
  hPutStr h "  movb %dil, -1(%rbp)\n"
  hPrintf h "  movq $%s, %%rax\n" syscallWrite
  hPrintf h "  movq $%s, %%rdi\n" fdStdOut
  hPutStr h "  leaq -1(%rbp), %rsi\n"
  hPutStr h "  movq $1, %rdx\n"
  hPutStr h "  syscall\n"
  hPutStr h "  leave\n"
  hPutStr h "  ret\n"
  hPrintf h "%s:\n" labelGetc
  hPutStr h "  pushq %rbp\n"
  hPutStr h "  movq %rsp, %rbp\n"
  hPutStr h "  subq $1, %rsp\n"
  hPrintf h "  movq $%s, %%rax\n" syscallRead
  hPrintf h "  movq $%s, %%rdi\n" fdStdIn
  hPutStr h "  leaq -1(%rbp), %rsi\n"
  hPutStr h "  movq $1, %rdx \n"
  hPutStr h "  syscall\n"
  hPutStr h "  movb -1(%rbp), %al\n"
  hPutStr h "  leave\n"
  hPutStr h "  ret\n"
  hPrintf h ".globl %s\n" labelMain
  hPrintf h "%s:\n" labelMain
  hPutStr h "  pushq %rbp\n"
  hPutStr h "  movq %rsp, %rbp\n"
  hPrintf h "  subq $%d, %%rsp\n" sizeOfFrame
  hPutStr h "  movq $0, -8(%rbp)\n"
  evalStateT (emit insts) (EmitState h [0] False 0)
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
ptrLoaded st = st { getPtrLoaded = True }

ptrUnloaded :: EmitState -> EmitState
ptrUnloaded st = st { getPtrLoaded = False }

loadPtr :: StateT EmitState IO ()
loadPtr = do loadNeeded <- gets (not . getPtrLoaded)
             when loadNeeded $ write "movq -8(%rbp), %rsi"
             modify ptrLoaded

genLoopNewId :: StateT EmitState IO Int
genLoopNewId = do st <- get
                  let newId =  getId st + 1
                  put $ st { getLoopStack = newId : getLoopStack st, getId = newId }
                  return newId

readLoopId :: StateT EmitState IO Int
readLoopId = gets $ head . getLoopStack

pushLoopId :: Int -> EmitState -> EmitState
pushLoopId label st = st { getLoopStack = label : getLoopStack st }

popLoopId :: EmitState -> EmitState
popLoopId st = case getLoopStack st of
                    [] -> st
                    (x:xs) -> st { getLoopStack = xs }

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
  loopId <- genLoopNewId
  loadPtr
  write $ "movb (%rsp,%rsi,1), %dil"
  write $ "cmpb $0, %dil"
  write $ "je " ++ labelLoopEnd loopId
  writeLabel $ labelLoopBegin loopId ++ ":"
  modify ptrUnloaded
  emit block
  emit insts
emit (Recur:insts) = do
  loopId <- readLoopId
  loadPtr
  write $ "movb (%rsp,%rsi,1), %dil"
  write $ "cmpb $0, %dil"
  write $ "jne " ++ labelLoopBegin loopId
  writeLabel $ labelLoopEnd loopId ++ ":"
  modify $ ptrUnloaded . popLoopId
  emit insts

