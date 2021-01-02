module Ristretto.Flow where
import Ristretto.AST
import Ristretto.Print
import qualified Data.Set as Set
import Data.Set(Set,(\\),union)
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map)
import Control.Monad.State
import Data.Functor((<$>))

{-
 x1 = a1; x2 = a2;
 x3 = x1; x1 = x2; x2 = x3;
 x3 = x1-x2; x4 = x1-x3; x5 = x3+x4; x6 = x5+x4
-}
example1 = [ 1 $= a 1
           , 2 $= a 2
           , 3 $= x 1
           , 1 $= x 2
           , 2 $= x 3
           , 3 $=- (x 1) $ (x 2)
           , 4 $=- (x 1) $ (x 3)
           , 5 $=+ (x 3) $ (x 4)
           , 6 $=+ (x 5) $ (x 4)
           , retint (x 6)
           ] where
             a = VParam
             x = VLocal

example2 = [ 1 $= c 7
           , retint (c 42)
           ] where c = VConst

-- This example checks for invalidation after reassignment
{- x1 = a1+a2; x2 = x1; x1 = a1 - a2; x3 = x2; ret x3 -}
example3 = [ 1 $=+ a 1 $ a 2
           , 2 $= x 1
           , 1 $=- a 1 $ a 2
           , 3 $= x 2
           , retint (x 3)
           ] where
             a = VParam
             x = VLocal

valVars :: Val -> [Int]
valVars (VLocal n) = [n]
valVars (VParam n) = [-n]
valVars _ = []

liveGen :: Instr -> [Int]
liveGen (Iret Nothing) = []
liveGen (Iret (Just v)) = valVars v
liveGen (Iassign n e) = exprVars e
liveGen Inop = []

exprVars (EVal v) = valVars v
exprVars (ECall _ vs) = concatMap valVars vs
exprVars (EOp _ v1 v2) = concatMap valVars [v1, v2]

liveKill (Iassign n v1) = [n]
liveKill _ = []

liveOut i liveIn = union (liveIn \\ kill) gen where
    gen  = Set.fromList $ liveGen i
    kill = Set.fromList $ liveKill i

live :: [Instr] -> Set Int
live [] = Set.empty
live (i:is) = liveOut i (live is)

liveSteps :: [Instr] -> [(Instr,Set Int)]
liveSteps = liveSteps' Set.empty

liveSteps' :: Set Int -> [Instr] -> [(Instr,Set Int)]
liveSteps' atEnd is = go is where
  go   []   = []
  go  [i]   = [(i,liveOut i liveIn)] where liveIn = atEnd
  go (i:is) =  (i,liveOut i liveIn):rest where
    rest = go is  -- nonempty
    liveIn = snd (head rest)

type CopyState = Map Int Val
type CopyM a = State CopyState a

insertCopy :: Int -> Int -> CopyM ()
insertCopy n1 n2 = modify (Map.insert n1 (VLocal n2))

replaceLocInVal :: CopyState -> Val -> Val
replaceLocInVal m v@(VLocal n) = case Map.lookup n m  of
  Nothing -> v
  Just v' -> v'
replaceLocInVal m v = v

replaceValStep :: Val -> CopyM Val
replaceValStep v = get >>= (return . flip replaceLocInVal v)

replaceExpStep :: Exp -> CopyM Exp
replaceExpStep (EOp op v1 v2) = do
  vs <- mapM replaceValStep [v1,v2]
  let [v1',v2'] = vs
  return $ EOp op v1' v2'
replaceExpStep (ECall name args) = do
    ECall name <$> mapM replaceValStep args

replaceExpStep (EVal v) = EVal <$> replaceValStep v

copyStep :: Instr -> CopyM Instr
copyStep (Iassign n (EVal v)) = do
  v' <- replaceValStep v
  modify (Map.insert n v')
  return (Iassign n (EVal v'))

copyStep (Iassign n exp) = do
  exp' <- replaceExpStep exp
  modify (Map.delete n)
  modify (Map.filter (/=VLocal n)) --invalidate uses of n
  return (Iassign n exp')

copyStep i = return i

copySteps :: [Instr] -> CopyM [Instr]
copySteps = mapM copyStep

propagate :: [Instr] -> [Instr]
propagate is = evalState (copySteps is) Map.empty


simpleDCE :: [Instr] -> [Instr]
simpleDCE [] = []
simpleDCE (i@(Iassign n _):is) | Set.member n (live is') = i:is'
                              | otherwise = is' where is' = simpleDCE is
simpleDCE (i:is) = i:simpleDCE is
