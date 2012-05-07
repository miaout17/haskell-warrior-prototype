module HaskellWarrior.Data where

import Data.Map (Map)
import qualified Data.Map as Map

data Vec = Vec Int Int deriving (Show, Eq, Ord)

vadd (Vec x1 y1) (Vec x2 y2) = Vec (x1+x2) (y1+y2)

data Dir = East | West deriving (Show)

-- TBD: North, South
toVec :: Dir -> Vec
toVec East = Vec 0 1
toVec West = Vec 0 (-1)

data Unit =
  PlayerUnit Int |
  SlimeUnit Int |
  ArcherUnit Int |
  WizardUnit Int |
  CatUnit Int |
  StairUnit |
  WallUnit
  deriving (Show, Eq)

unitChar :: Unit -> Char
unitChar (PlayerUnit _) = 'P'
unitChar (SlimeUnit _) = 's'
unitChar (ArcherUnit _) = 'a'
unitChar (WizardUnit _) = 'w'
unitChar (CatUnit _) = 'c'
unitChar StairUnit = '>'
unitChar WallUnit = '*' -- Unused

data UnitType = Player | Slime | Archer | Wizard | Cat | Stair | Wall | Empty deriving (Show, Eq)

unitType :: Maybe Unit -> UnitType
unitType (Just u) = unitType' u
unitType Nothing = Empty

-- Player
unitType' (PlayerUnit _) = Player
-- Enemies
unitType' (SlimeUnit _) = Slime
unitType' (ArcherUnit _) = Archer
unitType' (WizardUnit _) = Wizard
-- Friendly Creatures
unitType' (CatUnit _) = Cat
-- Non-combat
unitType' StairUnit = Stair
unitType' WallUnit = Wall

unitName :: Unit -> String
unitName unit = show $ unitType (Just unit)

data StageStatus = Running | Succeed | Failed deriving (Show)

type UnitMap = Map Vec Unit

data Stage = Stage {
  status :: StageStatus,
  turn :: Int,
  height :: Int,
  width :: Int,
  units :: UnitMap,

  messages :: [String]
} deriving (Show)

data StageState = StageState {
  feel :: Dir -> UnitType,
  look :: Dir -> (Int, UnitType),
  health :: Int
}
data Action = Rest | Walk Dir | Attack Dir | Rescue Dir | Shoot Dir

