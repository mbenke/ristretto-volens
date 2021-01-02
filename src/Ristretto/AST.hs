module Ristretto.AST where

type FunName = String
type Loc = Int

data Instr
  = Iret (Maybe Val)
  | Iassign Loc Exp
  | Iexp Exp
  | Inop
  -- deriving Show

infix 2 $=
n $= v = Iassign n (EVal v)

-- Usage: n $=- v1 $ v2
infix 2 $=-
($=-) :: Loc -> Val -> Val -> Instr 
($=-) n v1 v2 = Iassign n (EOp OpSub v1 v2)

infix 2 $=+
($=+) :: Loc -> Val -> Val -> Instr 
($=+) n v1 v2 = Iassign n (EOp OpAdd v1 v2)

retint = Iret . Just
retvoid = Iret Nothing

data Exp = EOp Op Val Val
         | ECall String [Val]
         | EVal Val -- this should be eliminated
         deriving Show
data Op = OpAdd | OpSub
instance Show Op where
    show OpAdd = "add"
    show OpSub = "sub"

data Val = VConst Int | VParam Int | VLocal Int
  deriving (Eq, Ord)
c_ = VConst
x_ = VLocal
a_ = VParam

instance Show Val where
  show (VConst n) = show n
  show (VParam n) = 'a':show n
  show (VLocal n) = 'x':show n


type Instrs = [Instr]

data FunDef = FunDef { fun_name :: FunName
                     , fun_locals :: Int
                     , fun_body :: Instrs
                     }
