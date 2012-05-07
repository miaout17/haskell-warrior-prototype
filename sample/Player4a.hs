module Player(action) where
import HaskellWarrior.Data

-- data Action    = Rest | Walk Dir | Attack Dir
-- data Dir       = East | West
-- data UnitType  = Wall | Empty | Stair | Slime | Archer
--
-- feel   :: StageState -> Dir -> UnitType
-- health :: StageState -> Int
-- look   :: StageState -> Dir -> (Int, UnitType)

action' :: UnitType -> Int -> Action
action' Slime _ = Attack East
action' _ health | health < 100 = Rest
action' _ _ = Walk East

action :: StageState -> Action
action state = action' u h
  where u = feel state East
        h = health state

