module Player(action) where
import HaskellWarrior.Data

-- data Action    = Rest | Walk Dir | Attack Dir | Shoot Dir
-- data Dir       = East | West
-- data UnitType  = Wall | Empty | Stair | Slime | Archer | Wizard
--
-- feel   :: StageState -> Dir -> UnitType
-- health :: StageState -> Int
-- look   :: StageState -> Dir -> (Int, UnitType)

action' :: Int -> UnitType -> Int -> Action
action' dist Wizard _
  | dist <= 2             = Walk West
  | dist == 3             = Shoot East
action' dist Archer hp
  | dist == 1             = Attack East
  | dist > 3 && hp <  100 = Rest
  | dist > 3 && hp >= 100 = Walk East
  | hp < 50               = Walk West
  | otherwise             = Walk East
action' 1 Slime _         = Attack East
action' _ _ hp | hp < 100 = Rest
action' _ _ _             = Walk East

action :: StageState -> Action
action state = action' dist unit hp
  where (dist, unit) = look state East
        hp = health state

