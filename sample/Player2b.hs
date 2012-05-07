module Player(action) where
import HaskellWarrior.Data

-- data Action    = Rest | Walk Dir | Attack Dir
-- data Dir       = East | West
-- data UnitType  = Wall | Empty | Slime | Stair
--
-- feel   :: StageState -> Dir -> UnitType
-- health :: StageState -> Int

action' :: UnitType -> Action
action' Slime = Attack East
action' _     = Walk East

action :: StageState -> Action
action state = action' (feel state East)

