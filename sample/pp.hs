module Player(action) where

import HaskellWarrior.Data


action' health Slime = Attack East
action' health Archer = Attack East
action' health unitType | health < 100 = Rest
action' health Empty = Walk East
action' health Stair = Walk East

action :: StageState -> Action
action state
  | f == Cat = Rescue East
  | f == Slime = Attack East
  | f == Archer = Attack East
  | shootWizard = Shoot East
  | nearWizard = Walk West
  | nearArcher && h<50 = Walk West
  | nearArcher && h>=50 = Walk East
  | h < 100 = Rest
  | otherwise = Walk East
  where
    h = health state
    f = feel state East
    l = look state East
    nearArcher =
      case l of
        Just (dist, Archer) -> dist<=3
        _ -> False
    nearWizard =
      case l of
        Just (dist, Wizard) -> dist<=2
        _ -> False
    shootWizard =
      case l of
        Just (3, Wizard) -> True
        _ -> False

