module Ristretto.AllocState where

import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Bifunctor
import Data.Either
import Data.Map.Strict(Map,(!))
import Data.Set (Set)
import qualified Data.Set as Set
import Lens.Micro
import Lens.Micro.Mtl(modifying, (%=))

import qualified PseudoAsm as PA
import PseudoAsm(Mem, Reg(..))
import Ristretto.AST


type Code = [PA.Ins]
type Location = Either Mem Reg

isReg :: Location -> Bool
isReg = isRight

data AState = AState {
  ast_regdesc :: RegisterMap,
  ast_valdesc :: ValueMap,
  ast_locals :: Int }

type RegisterMap = Map Reg RegisterDescr
type ValueMap = Map Val (Set Location)

type RegisterDescr = (RegState, Set Val)
data RegState
     = RegFree
     | RegUsed
     | RegLocked -- used not to alloc same reg twice in one transaction
     deriving(Eq, Ord, Show)

isRegFree, isRegLocked :: RegisterDescr ->  Bool
isRegFree = (RegFree==).fst
isRegLocked = (RegLocked==).fst

initState :: AState
initState = AState {
    ast_regdesc = Map.empty,
    ast_valdesc = Map.empty,
    ast_locals = 0 }

ast_regdescL :: Lens' AState RegisterMap
ast_regdescL = lens ast_regdesc (\s x -> s { ast_regdesc = x })

ast_valdescL :: Lens' AState ValueMap
ast_valdescL = lens ast_valdesc (\s x -> s { ast_valdesc = x })

modify_regdesc :: (RegisterMap -> RegisterMap) -> AM ()
modify_regdesc = modifying ast_regdescL

modify_valdesc :: (ValueMap -> ValueMap) -> AM ()
-- modify_valdesc f = modify (\s -> s { ast_valdesc = f (ast_valdesc s) })
modify_valdesc = modifying ast_valdescL


adjustValDesc :: (Set Location -> Set Location) -> Val -> AM ()
adjustValDesc f val = modifying ast_valdescL (Map.adjust f val)

addValLoc :: Val -> Location -> AM ()
addValLoc val loc = modifying ast_valdescL (Map.insertWith Set.union val (Set.singleton loc))

setValLoc :: Val -> Location -> AM ()
setValLoc val loc = adjustValDesc (const (Set.singleton loc)) val

adjustRegVal :: (Set Val -> Set Val) -> Reg -> AM ()
adjustRegVal f = modifying ast_regdescL . Map.adjust (second f)

addRegVal :: Reg -> Val -> AM ()
addRegVal reg val = adjustRegVal (Set.insert val) reg

setRegVal :: Reg -> Val -> AM ()
setRegVal reg val = adjustRegVal (\_ -> Set.singleton val) reg


type AM = StateT AState IO

runAM :: AM a -> IO (a, AState)
runAM m = runStateT m initState

evalAM :: AM a -> IO a
evalAM m = evalStateT m initState

debug :: String -> AM ()
debug = liftIO . putStrLn
