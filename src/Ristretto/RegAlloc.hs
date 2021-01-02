{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}
module Ristretto.RegAlloc where

import Control.Monad.State
import Data.List(intercalate)
import Data.Maybe
import qualified Data.Map.Strict as Map
import Data.Map.Strict(Map,(!))
import Data.Set (Set)
import qualified Data.Set as Set

import Ristretto.AllocState
import Ristretto.AST
import Ristretto.Flow
import qualified PseudoAsm as PA
import PseudoAsm(Mem, Reg(..))

import Debug.Trace
showTrace :: Show b => b -> a -> a
showTrace b = trace (show b)

class HasRegs a where
  hasRegs :: a -> [Reg]

instance HasRegs Reg where
  hasRegs r = [r]

instance HasRegs a => HasRegs [a] where
  hasRegs = concatMap hasRegs

instance (HasRegs b) => HasRegs (Either a b) where
  hasRegs (Left a) = []
  hasRegs (Right b) = hasRegs b

instance HasRegs a => HasRegs (Set a) where
  hasRegs = hasRegs . Set.toList

class HasArgs a where
  hasArgs :: a -> [Int]

instance HasArgs Val where
  hasArgs (VParam n ) = [n]
  hasArgs _ = []

instance HasArgs Instr where
  hasArgs i = go i where
    go (Iret (Just v)) = hasArgs v
    go i@(Iassign x (EOp op v1 v2)) = hasArgs v1 ++ hasArgs v2
    go _ = [] -- FIXME

instance HasArgs a => HasArgs (Maybe a) where
  -- hasArgs = foldMap hasArgs
  hasArgs = maybe [] hasArgs

instance HasArgs a => HasArgs [a] where
  hasArgs = concatMap hasArgs


listFreeRegs :: RegisterMap -> [Reg]
listFreeRegs = map fst . filter (isRegFree . snd) . Map.toList

listNotLockedRegs :: RegisterMap -> [Reg]
listNotLockedRegs = map fst . filter (not . isRegLocked . snd) . Map.toList

getNotLockedRegs :: AM [Reg]
getNotLockedRegs =  gets (listNotLockedRegs . ast_regdesc)  

setRegState :: RegState -> Reg -> AM ()
setRegState RegFree reg = do
  modify_regdesc (Map.adjust modifier reg)
  -- debug $ "free " ++ show reg
    where
      modifier (_, vals) = (RegFree, Set.empty)

setRegState newState reg =
  modify_regdesc (Map.adjust modifier reg)
    where
      modifier (_, vals) = (newState, vals)

lockReg :: Reg -> AM ()
lockReg = setRegState RegLocked

setFree :: Reg -> AM ()
setFree = setRegState RegFree

unlockReg :: Reg -> AM ()
unlockReg reg = do
  regMap <- gets ast_regdesc
  modify_regdesc (Map.adjust modifier reg) where
    modifier (_, vals) = (if Set.null vals then RegFree else RegUsed, vals)

lockRegs :: HasRegs a => a -> AM ()
lockRegs a = mapM_ lockReg (hasRegs a)

unlockRegs :: HasRegs a => a -> AM ()
unlockRegs a = mapM_ unlockReg (hasRegs a)

getFreeReg :: AM (Reg, Code)
getFreeReg = do
  regMap <- gets ast_regdesc
  case maybeHead $ listFreeRegs regMap of
    Just r -> lockReg r >> return (r, [])
    Nothing -> spillAnyReg

type LiveSet = Set Int

showLiveSet = intercalate ", " . map showLive . Set.toList

showLive :: Int -> String
showLive n
  | n > 0 = 'x':show n
  | n < 0 = 'a':show (negate n)

liveIn :: Val -> LiveSet -> Bool
liveIn (VLocal n) set = Set.member n set
liveIn (VParam n) set = Set.member (negate n) set

genInstr :: Instr -> LiveSet -> LiveSet -> AM Code
genInstr (Iret Nothing) _ _ = return [PA.Ret]
genInstr (Iret (Just p@(VParam n))) _ _ = do
  loc <- getValLoc' (Just $ PA.Reg 0) p
  return $ setRetVal loc++[PA.Ret]
genInstr (Iret (Just p@(VLocal n))) _ _ = do
  loc <- getValLoc' (Just $ PA.Reg 0) p
  let code = setRetVal loc++[PA.Ret]
  debugEmit code
  return code

genInstr i@(Iassign x (EOp op v1 v2)) liveBefore liveAfter = do
   debug $ unwords ["quad:", show i, "   liveAfter:", showLiveSet liveAfter]
   loc1 <- getValLoc v1
   (r1, code1) <- getIntoReg loc1
   debugEmit code1
   setRegVal r1 v1
   debug $ unwords[show v1, "in", show r1]
   addValLoc v1 (Right r1)
   setRegState RegLocked r1  -- prevent spilling r1 when loading r2
   loc2 <- getValLoc v2
   (r2, code2) <- getIntoReg loc2
   debugEmit code2
   debug $ unwords[show v2, "in", show r2]
   setRegVal r2 v2
   addValLoc v2 (Right r2)
   -- prevent spilling r2 when looking for a free reg
   if v1 `liveIn` liveAfter then setRegState RegLocked r1 else setRegState RegFree r1
   if v2 `liveIn` liveAfter then setRegState RegLocked r2 else setRegState RegFree r2
   (r0, spillCode) <- getFreeReg
   debugEmit spillCode
   let code3 = [PA.Op (show op) r0 r1 r2]
   setRegVal r0 (VLocal x)
   debugEmit code3
   debug $ unwords[show $ VLocal x, "in", show r0]
   addValLoc (VLocal x) (Right r0)
   if (v1 `liveIn` liveAfter) || r1 == r0 then setRegState RegUsed r1 else setRegState RegFree r1
   if (v2 `liveIn` liveAfter) || r2 == r0 then setRegState RegUsed r2 else setRegState RegFree r2
   if VLocal x `liveIn` liveAfter then setRegState RegUsed r0 else setRegState RegFree r0
   -- forM_ [(VLocal x, r0), (v1,r1), (v2,r2)]
   --   $ \(v,r) -> flip setRegState r $ if v `liveIn` liveAfter then RegUsed else RegFree   
   return $ code1++code2++spillCode++code3 -- FIXME

genInstr i _ _ = error $ "unimplemented: " ++ show i

getIntoReg :: Location -> AM (Reg, Code)
getIntoReg (Right r) = return (r, [])
getIntoReg (Left m) = do
  (r, spillCode) <- getFreeReg
  let code = [PA.Load r m]
  return (r, spillCode++code)

spillAnyReg :: AM (Reg, Code)
spillAnyReg = do
  regs <- getNotLockedRegs
  case regs of
    [] -> error "spillAnyReg: all regs are locked"
    (r:rs) -> do         -- TODO better register selection
      code <- spillReg r
      return (r, code)

spillReg :: Reg -> AM Code
spillReg r = do
  debug $ "spillReg "++show r
  regdesc <- gets ast_regdesc
  case Map.lookup r regdesc of
    Nothing -> return mempty
    Just (state, valueSet) -> do
      let values = Set.toList valueSet
      spillCodes <- mapM (spillVal r) values
      setRegState RegFree r
      return (concat spillCodes)

spillVal :: Reg -> Val -> AM Code
spillVal r val = do
  debug $ unwords["spillVal", show val, "from", show r]
  adjustValDesc (Set.delete (Right r)) val
  valdesc <- gets ast_valdesc
  let remainingLocations = fromJust $ Map.lookup val valdesc
  if null remainingLocations
    then do
      addr <- findLocToStoreVal val
      setValLoc val (Left addr)
      return [PA.Store r addr]
    else
      return []

findLocToStoreVal :: Val -> AM Int
findLocToStoreVal (VLocal n) = return $ (-4)*n

genBlock = genBlock' registers Set.empty where
  registers = map Reg [0..2]

genBlock' :: [Reg]-> LiveSet -> [Instr] -> AM [PA.Ins]
genBlock' registers liveAfterBlock is = do
    let flow = liveSteps is
    init is
    -- concat <$> mapM genInstr flow
    go flow
  where
    go :: [(Instr, LiveSet)] -> AM [PA.Ins]
    go [] = return []
    go [(i, liveBefore)] = genInstr i liveBefore liveAfterBlock
    go ((i, liveBefore):flow@((_, liveAfter):_)) = do
      c <- genInstr i liveBefore liveAfter
      cs <- go flow
      return $ c ++ cs

    init :: [Instr] -> AM ()
    init is = do
         let argc = blockArgCount is
         let ad n = (VParam n, Set.singleton (Left $ PA.argAddr n))
         let ads = [ad n | n <- [1..argc]]
         modify (\s -> s { ast_valdesc = Map.fromList ads })
         let initRegDescrs = [(r, (RegFree, Set.empty)) | r <- registers]
         modify (\s -> s { ast_regdesc = Map.fromList initRegDescrs })

blockArgCount b = maximum (hasArgs b)


type PreferredReg = Maybe PA.Reg

getValLoc :: Val -> AM Location
getValLoc = getValLoc' Nothing

getValLoc' :: PreferredReg -> Val -> AM Location
getValLoc' prefer val = do
  valDesc <- gets ast_valdesc
  case Map.lookup val valDesc >>= lookupLoc val prefer of
    Nothing -> error $ show val ++ " not found in desc\n" ++ "valdesc: " ++ show valDesc
    Just (loc::Location) -> return loc

lookupLoc :: Val -> PreferredReg -> Set Location -> Maybe Location
lookupLoc v pr set
  = anyOne  $ if Set.null regLocs
    then set
    else regLocs where
      regLocs = Set.filter isReg set

setRetVal :: Location -> [PA.Ins]
setRetVal (Left m) = [PA.Load (Reg 0) m]
setRetVal (Right (PA.Reg n))
  | n == 0 = []
  | otherwise = [PA.Move (Reg 0) (Reg n)]

testBlock1 = [Iret Nothing]
testBlock2 = [retint (VParam 1)]
-- testBlock3 = [Iassign 1 (EOp OpAdd (VParam 1) (VParam 2)), retint (VLocal 1)]
testBlock3 = [ 1 $=+ arg 1 $ arg 2, retint (x 1) ] where (arg, x) = (VParam, VLocal)
testBlock3b = [ 1 $=+ arg 1 $ arg 1, retint (x 1) ] where (arg, x) = (VParam, VLocal)
testBlock4 =
  [ 1 $=+ arg 1 $ arg 2
  , 2 $=+ (arg 1) $ (arg 3)
  , 3 $=+ (arg 2) $ (arg 3)
  , 4 $=+ x 1 $ x 2
  , 5 $=+ x 3 $ x 4 
  , retint (x 4)
  ] where (arg, x) = (VParam, VLocal)

test b = do
  res <- evalAM (genBlock b)
  let out = unlines $ map show res
  -- writeln "Final code:"
  -- writeln out
  return ()
  
runTests :: IO ()
runTests = do
  test testBlock4

  
maybeHead :: [a] -> Maybe a
maybeHead (x:_) = Just x
maybeHead [] = Nothing


anyOne :: Set a -> Maybe a
anyOne = maybeHead . Set.toList

writeln :: String -> IO ()
writeln = putStrLn

debugEmit :: [PA.Ins] -> AM ()
debugEmit code = unless (null code) $ debug $ "emit: " ++ intercalate "\n" (map show code)
