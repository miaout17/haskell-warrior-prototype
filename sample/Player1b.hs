module Player(action) where
import HaskellWarrior.Data

-- data Action = Rest | Walk Dir | Attack Dir
-- data Dir = East | West
-- data UnitType = Wall | Empty | Slime | Stair
--
-- feel :: StageState -> Dir -> UnitType
-- health :: StageState -> Int

action :: StageState -> Action
action state = Walk East

