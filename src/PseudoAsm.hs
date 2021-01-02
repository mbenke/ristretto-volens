module PseudoAsm where

data Reg = Reg Int deriving(Eq,Ord)
instance Show Reg where
    show (Reg n) = 'R':show n

type Mem = Int  

data Ins
     = Op String Reg Reg Reg
     | Load Reg Mem
     | Store Reg Mem
     | Ret
     | Move Reg Reg
     | MoveImm Reg Int
     | OpImm Reg Reg Int
     
-- TODO: handle `R0 := n - R1` e.g. ARM has `RSB` instruction: `RSB r0, r1, n`

showFPrel :: Int -> String
showFPrel n
    | n < 0 = "[FP"++show n++"]"
    | True  = "[FP+"++show n++"]"
    
instance Show Ins where
    show (Op name r0 r1 r2) = concat [show r0, " := ", name, " ", show r1, ", ", show r2]
    show (Load r0 d) = concat ["MOV ", show r0, ", ",showFPrel d]
    show (Store r0 d) = concat ["MOV ", showFPrel d, ", ",show r0]
    show Ret = "RET"
    show (Move r0 r1) = concat ["MOV ", show r0, ", ", show r1]

argAddr :: Int -> Int
argAddr n = 4 + 4*n

