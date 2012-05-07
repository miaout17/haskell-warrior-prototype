module HaskellWarrior.Renderer(
  renderStage
) where

import Data.Map (Map)
import qualified Data.Map as Map

import HaskellWarrior.Data

renderCell :: Stage -> Int -> Int -> Char
renderCell stage x y =
  case (Map.lookup (Vec x y) (units stage)) of
    Just unit -> unitChar unit
    Nothing -> ' '

renderRow :: Stage -> Int -> String
renderRow stage x =
  map (renderCell stage x) [0..(width stage - 1)]

renderStage' :: Stage -> [String]
renderStage' stage =
  map (renderRow stage) [0..(height stage - 1)]

renderStage :: Stage -> String
renderStage stage =
  foldl (++) [] lines -- TODO
  where
  w = width stage
  wall = "*" ++ replicate (width stage) '-' ++ "*\n"
  centerLines = foldl (++) [] $ map (\s -> '|' : s ++ "|\n") (renderStage' stage)
  lines = wall : centerLines : [wall]

