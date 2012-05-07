module HaskellWarrior.Engine(
  Stage(..),
  run
) where

import Data.List (find)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Concurrent (threadDelay)

import Control.Monad.State
import Control.Monad(when)

import HaskellWarrior.Data
import HaskellWarrior.Renderer
import HaskellWarrior.StageData

import System(getArgs)

import qualified Player(action)

type StageMonad = State Stage

maxTurn = 1000
maxPlayerHP = 100
restRecoveryHP = 10

-- TODO: Bad complexity, need refactoring data structure
player :: Stage -> Maybe (Vec, Unit)
player stage = Map.foldrWithKey foldFunc Nothing (units stage)
  where
    foldFunc :: Vec -> Unit -> Maybe (Vec, Unit) -> Maybe (Vec, Unit)
    foldFunc _ _ (Just a) = Just a
    foldFunc vec unit@(PlayerUnit _) _ = Just (vec, unit)
    foldFunc _ _ Nothing = Nothing

appendMessage :: String -> StageMonad ()
appendMessage newMessage = modify $ \s -> s{messages = messages s ++ [newMessage]}

modifyUnits :: (UnitMap -> UnitMap) -> StageMonad ()
modifyUnits f = modify $ \s -> s{units = f (units s)}

moveUnit :: Vec -> Vec -> StageMonad ()
moveUnit from to = modifyUnits $ moveUnit' from to
  where
    moveUnit' :: Vec -> Vec -> (UnitMap -> UnitMap)
    moveUnit' from to map =
      let
        Just unit = Map.lookup from map
        map' = Map.delete from map
      in
        Map.insert to unit map'

processPlayerAction :: Action -> StageMonad ()
processPlayerAction Rest = do
  Just (pos, PlayerUnit health) <- gets player
  let
    newHealth = min maxPlayerHP (health + restRecoveryHP)
    gainHealth = newHealth - health
  modifyUnits $ Map.insert pos (PlayerUnit newHealth)
  appendMessage $ "The warrior rests and gain " ++ show gainHealth ++ " HP and has " ++ show newHealth ++ " HP now"

processPlayerAction (Walk dir) = do
  Just (pos, PlayerUnit health) <- gets player
  let
    dirName = show dir
    offset = toVec dir
    newPos = vadd pos offset
  targetUnit <- gets $ (Map.lookup newPos) . units
  case targetUnit of
    Just StairUnit -> do
      appendMessage $ "The warrior walks to the stairs"
      modify $ \s -> s{status=Succeed}
    Just _ ->
      appendMessage $ "The warrior walks " ++ dirName ++ " but bumped something"
    Nothing -> do
      moveUnit pos newPos
      appendMessage $ "The warrior walks " ++ dirName

processPlayerAction (Attack dir) = do
  Just (pos, PlayerUnit health) <- gets player
  us <- gets units
  let
    target :: Maybe (Int, Vec, Unit)
    target = lookUnit us pos dir 1
    damage = 25
  case target of
    Just (_, enemyPos, enemyUnit@(CatUnit _)) -> dontAttackCuteCat
    Just (_, enemyPos, enemyUnit@(SlimeUnit enemyHealth)) -> playerAttackResult enemyPos damage enemyHealth SlimeUnit "attacked"
    Just (_, enemyPos, enemyUnit@(ArcherUnit enemyHealth)) -> playerAttackResult enemyPos damage enemyHealth ArcherUnit "attacked"
    Just (_, enemyPos, enemyUnit@(WizardUnit enemyHealth)) -> playerAttackResult enemyPos damage enemyHealth WizardUnit "attacked"
    _ -> appendMessage "The warrior tried to attack, but there is nothing attackable"

processPlayerAction (Shoot dir) = do
  Just (pos, PlayerUnit health) <- gets player
  us <- gets units
  let
    target :: Maybe (Int, Vec, Unit)
    target = lookUnit us pos dir 3
    damage = 10
  case target of
    Just (_, enemyPos, enemyUnit@(CatUnit _)) -> dontAttackCuteCat
    Just (_, enemyPos, enemyUnit@(WizardUnit enemyHealth)) -> playerAttackResult enemyPos damage enemyHealth WizardUnit "shooted"
    Just (_, enemyPos, enemyUnit@(SlimeUnit enemyHealth)) ->
      appendMessage "The warrior tried to shoot the slime, but the slime absorbed the arrow"
    Just (_, enemyPos, enemyUnit@(ArcherUnit enemyHealth)) ->
      appendMessage "The warrior tried to shoot the archer, but the archer dodged"
    _ -> appendMessage "The warrior tried to shoot, but there is nothing to shoot"

processPlayerAction (Rescue dir) = do
  Just (pos, PlayerUnit health) <- gets player
  us <- gets units
  let
    target :: Maybe (Int, Vec, Unit)
    target = lookUnit us pos dir 1
  case target of
    Just (_, catPos, CatUnit _) -> do
      appendMessage "The warrior rescued a cute cat. Good Job!"
      modifyUnits $ Map.delete catPos
    _ -> appendMessage "The warrior tried to rescue something, but there is nothing to rescue"

dontAttackCuteCat = do
  appendMessage "Oh no. You attacked a cute cat. How cruel are you? GAME OVER!!!!"
  modify $ \s -> s{status=Failed}

-- playerAttackResult enemyPos damage enemyHealth constructor
playerAttackResult :: Vec -> Int -> Int -> (Int -> Unit) -> String -> StageMonad ()
playerAttackResult pos damage health constructor verb = do
  let
    newHealth = health - damage
    newEnemy = constructor newHealth
    defenserName = unitName newEnemy
  appendMessage $ "The warrior " ++ verb ++ " " ++ defenserName ++ ", dealing " ++ show damage ++ " damage"
  if newHealth > 0 then do
    modifyUnits $ Map.insert pos (constructor newHealth)
  else do
    appendMessage $ defenserName ++ " died"
    modifyUnits $ Map.delete pos

-- playerAttackResult :: Vec -> Unit -> StageMonad ()
-- playerAttackResult pos unit@(SlimeUnit health) = do
--   let
--     damage = 25
--     newHealth = health - damage
--   modifyUnits $ Map.insert pos (PlayerUnit newHealth)

allPositions stage =
  [Vec x y | x<-[0..h-1], y<-[0..w-1]]
  where
    h = height stage
    w = width stage

-- lookUnit pos dir range = Just (distance pos unit)
lookUnit :: UnitMap -> Vec -> Dir -> Int -> Maybe (Int, Vec, Unit)
lookUnit units pos dir range =
  lookUnit' units nextPos dir (range-1) 1
  where
    offset = toVec dir
    nextPos = vadd pos offset

lookUnit' units pos dir range dist | range<0 = Nothing
lookUnit' units pos dir range dist | range>=0 =
  case maybeUnit of
    Just unit -> Just (dist, pos, unit)
    Nothing -> lookUnit' units nextPos dir (range-1) (dist+1)
  where
    maybeUnit = Map.lookup pos units
    offset = toVec dir
    nextPos = vadd pos offset

enemyAttackResult :: Unit -> Int -> StageMonad ()
enemyAttackResult attacker damage = do
  Just (pos, PlayerUnit health) <- gets player
  let
    attackerName = unitName attacker
    newHealth = health - damage
  appendMessage $ attackerName ++ " attacked the warrior, dealing " ++ show damage ++ " damage. Now HP=" ++ show newHealth
  if newHealth > 0 then do
    modifyUnits $ Map.insert pos (PlayerUnit newHealth)
  else do
    appendMessage "The Haskell Warrior DIED...."
    modifyUnits $ Map.delete pos
    modify $ \s -> s{status=Failed}

processEnemyAction :: Vec -> Unit -> Int -> Int -> StageMonad ()
processEnemyAction pos unit damage range = do
  us <- gets units
  let
    targets = map (\dir -> lookUnit us pos dir range) [West, East]

    checkPlayer :: Maybe (Int, Vec, Unit) -> Bool
    checkPlayer (Just (_, _, PlayerUnit _)) = True
    checkPlayer _ = False

    target = join $ find checkPlayer targets

  case target of
    Just (_, pos, target@(PlayerUnit health)) ->
      enemyAttackResult unit damage
    Nothing -> return ()

processUnitAction :: (Vec, Unit) -> StageMonad ()
processUnitAction (pos, unit@(SlimeUnit _)) = processEnemyAction pos unit 12 1
processUnitAction (pos, unit@(ArcherUnit _)) = processEnemyAction pos unit 12 3
processUnitAction (pos, unit@(WizardUnit _)) = processEnemyAction pos unit 40 2

processUnitAction _ = return ()

stageState :: Stage -> StageState
stageState stage = StageState {
  feel = feel',
  look = look',
  health = health
} where
  us = units stage
  Just (playerPos, PlayerUnit health) = player stage
  feel' dir = unitType maybeUnit
    where maybeUnit = case lookUnit us playerPos dir 1 of
                        Just (dist, pos, unit) -> Just unit
                        Nothing -> Nothing
  look' dir = case lookUnit us playerPos dir maxBound of
                Just (dist, pos, unit) -> (dist, unitType (Just unit))
                _ -> error "This should not happen. The stage should bound by wall"

processTurn :: StageMonad ()
processTurn = do
  modify $ \s -> s{turn=(turn s+1)}
  stage <- get
  let action = Player.action $ stageState stage
  processPlayerAction action
  us <- gets units
  mapM_ processUnitAction $ Map.toList us
  return ()

postTurn :: StageMonad ()
postTurn = do
  t <- gets $ turn
  when (t>=maxTurn) (modify $ \s -> s{status=Failed})

run = do
  args <- getArgs
  let
    defaultStageID = 1
    defaultDelay = 0.3
    (stageID, delay) =
      case args of
        [] -> (defaultStageID, defaultDelay)
        [stageID] -> (read stageID, defaultDelay)
        [stageID, delay] -> (read stageID, read delay)
        _ -> error "usage: ./Main [stageID] [delay]"
    stage = newStage stageID
  runLoop stage delay

sleep :: Double -> IO ()
sleep sec = threadDelay (floor $ sec * 1000000)

runLoop stage delay = do
  putStr $ "\n- Turn " ++ show (turn stage) ++ " -\n"
  putStr $ renderStage stage
  let stage' = execState processTurn stage
  mapM_ putStrLn $ messages stage'
  let newStage = execState postTurn stage'
  case (status newStage) of
    Running -> do
      when (delay>0) (sleep delay)
      runLoop newStage{messages=[]} delay
    Succeed -> putStrLn "Quest completed!!"
    Failed -> putStrLn "Quest failed!!"

