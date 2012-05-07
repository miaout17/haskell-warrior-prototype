module HaskellWarrior.StageData(newStage) where


import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe(isJust)

import HaskellWarrior.Data

stageStr :: Int -> String
stageStr 1 = "P             >"
stageStr 2 = "P  s          >"
stageStr 3 = "P  s  s  s    >"
stageStr 4 = "P      a    ss>"
stageStr 5 = "P    sw       >"
stageStr _ = "P             >"

charToUnit :: Char -> Maybe Unit
charToUnit 'P' = Just $ PlayerUnit 100
charToUnit 's' = Just $ SlimeUnit 100
charToUnit 'a' = Just $ ArcherUnit 45
charToUnit 'w' = Just $ WizardUnit 100
charToUnit 'c' = Just $ CatUnit 999
charToUnit '>' = Just $ StairUnit
charToUnit _ = Nothing

loadStage :: Int -> (Int, [(Vec, Unit)])
loadStage levelID =
  let
    sstr = stageStr levelID
    maybeUnits = map charToUnit sstr
    indexed = zip [0..] maybeUnits
    filtered = filter (isJust . snd) indexed
    units = map (\(idx, Just unit) -> (Vec 0 idx, unit)) filtered
    width = length sstr
  in
    (width, units)

newStage levelID = Stage Running 1 h w allUnits []
  where
    h = 1
    (w, normalUnits) = loadStage levelID
    wallVecs = [Vec x y | x<-[-1, h], y<-[-1..w]] ++
               [Vec x y | y<-[-1, w], x<-[-1..h]]
    wallUnits = map (\v -> (v, WallUnit)) wallVecs
    allUnits = Map.fromList $ normalUnits ++ wallUnits
