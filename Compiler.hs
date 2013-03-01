module Compiler where

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
