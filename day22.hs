
import System.IO (readFile)
import Control.Monad.Trans.Writer.Lazy (Writer, tell, runWriter)
import Control.Monad (guard)
import qualified Data.Heap as Heap

--

data Spell = MagicMissile | Drain | Shield | Poison | Recharge deriving Eq

instance Show Spell where
  show MagicMissile = "Magic Missile"
  show Drain = "Drain"
  show Shield = "Shield"
  show Poison = "Poison"
  show Recharge = "Recharge"

cost :: Spell -> Mana
cost MagicMissile = 53
cost Drain = 73
cost Shield = 113
cost Poison = 173
cost Recharge = 229

spellDescription :: Spell -> String
spellDescription MagicMissile = ", dealing 4 damage"
spellDescription Drain = ", dealing 2 damage, and healing 2 hit points"
spellDescription Shield = ", increasing armor by 7"
spellDescription Poison = ""
spellDescription Recharge = ""

type Mana = Int
type HP = Int
type ShieldTimer = Int
type PoisonTimer = Int
type RechargeTimer = Int

data State = Win
           | Ongoing Mana HP HP (ShieldTimer, PoisonTimer, RechargeTimer)
           | Lose
           deriving (Eq, Show, Ord)

getMana :: State -> Mana
getMana (Ongoing mana _ _ _) = mana

getPlayerHp :: State -> HP
getPlayerHp (Ongoing _ playerHp _ _) = playerHp

getEnemyHp :: State -> HP
getEnemyHp (Ongoing _ _ enemyHp _) = enemyHp

getShieldTimer :: State -> ShieldTimer
getShieldTimer (Ongoing _ _ _ (shieldTimer, _, _)) = shieldTimer

getPoisonTimer :: State -> PoisonTimer
getPoisonTimer (Ongoing _ _ _ (_, poisonTimer, _)) = poisonTimer

getRechargeTimer :: State -> RechargeTimer
getRechargeTimer (Ongoing _ _ _ (_, _, rechargeTimer)) = rechargeTimer

--

displayStatus :: State -> Writer [String] State
displayStatus state = do
  let playerHp = getPlayerHp state
  let playerArmor = if getShieldTimer state > 0 then 7 else 0
  let mana = getMana state
  let playerHpPlural = if playerHp == 1 then "" else "s"
  tell ["- Player has " ++ show playerHp ++ " hit point" ++ playerHpPlural ++ ", " ++ show playerArmor ++ " armor, " ++ show mana ++ " mana"]
  let enemyHp = getEnemyHp state
  let enemyHpPlural = if enemyHp == 1 then "" else "s"
  tell ["- Boss has " ++ show enemyHp ++ " hit point" ++ enemyHpPlural]
  return state

advance :: Spell -> Int -> Int -> State -> Writer [String] State
advance spell bossDamage healthLossPerTurn state = do
  let mana = getMana state
  let playerHp = getPlayerHp state
  let enemyHp = getEnemyHp state
  let initialShieldTimer = getShieldTimer state
  let initialPoisonTimer = getPoisonTimer state
  let initialRechargeTimer = getRechargeTimer state
  let manaAfterCasting = mana - cost spell
  stateAfterCasting <- castSpell spell (Ongoing manaAfterCasting playerHp enemyHp (initialShieldTimer, initialPoisonTimer, initialRechargeTimer))
  if stateAfterCasting == Win
    then
      return Win
    else do
      tell [""]
      tell ["-- Boss turn --"]
      displayStatus stateAfterCasting
      stateAfterEffectsBeforeBossAttack <- applyEffects stateAfterCasting
      if stateAfterEffectsBeforeBossAttack == Win
        then
          return Win
        else do
          stateAfterBossAttack <- bossAttack stateAfterEffectsBeforeBossAttack bossDamage
          if stateAfterBossAttack == Lose
            then
              return Lose
            else do
              tell [""]
              tell ["-- Player turn --"]
              let playerHpAfterAttrition = getPlayerHp stateAfterBossAttack - healthLossPerTurn
              if playerHpAfterAttrition <= 0
                then
                  return Lose
                else do
                  let stateAfterAttrition = Ongoing (getMana stateAfterBossAttack) playerHpAfterAttrition (getEnemyHp stateAfterBossAttack) (getShieldTimer stateAfterBossAttack, getPoisonTimer stateAfterBossAttack, getRechargeTimer stateAfterBossAttack)
                  displayStatus stateAfterAttrition
                  applyEffects stateAfterAttrition

bossAttack :: State -> Int -> Writer [String] State
bossAttack state bossDamage = do
  let mana = getMana state
  let playerHp = getPlayerHp state
  let enemyHp = getEnemyHp state
  let shieldTimer = getShieldTimer state
  let poisonTimer = getPoisonTimer state
  let rechargeTimer = getRechargeTimer state
  let playerArmor = if getShieldTimer state > 0 then 7 else 0
  let damageDealt = max (bossDamage - playerArmor) 1
  let newPlayerHp = playerHp - damageDealt
  let newState = if newPlayerHp <= 0 then Lose else Ongoing mana newPlayerHp enemyHp (shieldTimer, poisonTimer, rechargeTimer)
  let damageCalculation = if playerArmor /= 0 then show bossDamage ++ " - " ++ show playerArmor ++ " = " else ""
  let loseClause = if newState == Lose then " This kills the player, and the boss wins." else ""
  tell ["Boss attacks for " ++ damageCalculation ++ show damageDealt ++ " damage!" ++ loseClause]
  return newState

applyEffects :: State -> Writer [String] State
applyEffects state = do
  let playerHp = getPlayerHp state
  let mana = getMana state
  let enemyHp = getEnemyHp state
  let initialShieldTimer = getShieldTimer state
  let initialPoisonTimer = getPoisonTimer state
  let initialRechargeTimer = getRechargeTimer state
  let newShieldTimer = max (initialShieldTimer - 1) 0
  if initialShieldTimer > 0
    then do
      tell ["Shield's timer is now " ++ show newShieldTimer ++ "."]
      if newShieldTimer == 0
        then
          tell ["Shield wears off, decreasing armor by 7."]
        else
          return ()
    else
      return ()
  let newEnemyHp = enemyHp - (if initialPoisonTimer > 0 then 3 else 0)
  if newEnemyHp <= 0
    then do
      tell ["Poison deals 3 damage. This kills the boss, and the player wins."]
      return Win
    else do
      let newPoisonTimer = max (initialPoisonTimer - 1) 0
      if initialPoisonTimer > 0
        then do
          tell ["Poison deals 3 damage; its timer is now " ++ show newPoisonTimer ++ "."]
          if newPoisonTimer == 0
            then
              tell ["Poison wears off."]
            else
              return ()
        else
          return ()
      let newMana = mana + (if initialRechargeTimer > 0 then 101 else 0)
      let newRechargeTimer = max (initialRechargeTimer - 1) 0
      if initialRechargeTimer > 0
        then do
          tell ["Recharge provides 101 mana; its timer is now " ++ show newRechargeTimer ++ "."]
          if newRechargeTimer == 0
            then
              tell ["Recharge wears off."]
            else
              return ()
        else
          return ()
      return (Ongoing newMana playerHp newEnemyHp (newShieldTimer, newPoisonTimer, newRechargeTimer))

castSpell :: Spell -> State -> Writer [String] State
castSpell spell state = do
  let newState = castSpell' spell state
  let winClause = if newState == Win then " This kills the boss, and the player wins." else ""
  tell ["Player casts " ++ show spell ++ spellDescription spell ++ "." ++ winClause]
  return newState

castSpell' :: Spell -> State -> State
castSpell' MagicMissile (Ongoing mana playerHp enemyHp timers)
  = if enemyHp - 4 <= 0 then Win else Ongoing mana playerHp (enemyHp - 4) timers
castSpell' Drain (Ongoing mana playerHp enemyHp timers)
  = if enemyHp - 2 <= 0 then Win else Ongoing mana (playerHp + 2) (enemyHp - 2) timers
castSpell' Shield (Ongoing mana playerHp enemyHp (0, poisonTimer, rechargeTimer))
  = Ongoing mana playerHp enemyHp (6, poisonTimer, rechargeTimer)
castSpell' Poison (Ongoing mana playerHp enemyHp (shieldTimer, 0, rechargeTimer))
  = Ongoing mana playerHp enemyHp (shieldTimer, 6, rechargeTimer)
castSpell' Recharge (Ongoing mana playerHp enemyHp (shieldTimer, poisonTimer, 0))
  = Ongoing mana playerHp enemyHp (shieldTimer, poisonTimer, 5)

--

possibleSpells :: State -> [Spell]
possibleSpells Lose = []
possibleSpells state = do
  spell <- [MagicMissile, Drain, Shield, Poison, Recharge]
  let availableMana = getMana state
  guard $ cost spell <= availableMana
  guard $ getShieldTimer state == 0 || spell /= Shield
  guard $ getPoisonTimer state == 0 || spell /= Poison
  guard $ getRechargeTimer state == 0 || spell /= Recharge
  return spell

winSpendingLeastMana :: State -> Int -> Int -> Int
winSpendingLeastMana state enemyDamage healthLossPerTurn = winSpendingLeastMana' (Heap.singleton (0, state)) enemyDamage healthLossPerTurn

winSpendingLeastMana' :: Heap.MinHeap (Int, State) -> Int -> Int -> Int
winSpendingLeastMana' minDistances enemyDamage healthLossPerTurn = if state == Win then dist else winSpendingLeastMana' heapWithNewStatesInserted enemyDamage healthLossPerTurn
  where
    (Just ((dist, state), heapWithLowestRemoved)) = Heap.view minDistances
    availableSpells = possibleSpells state
    advancedStatesAndDists = map (\spell -> (dist + cost spell, fst $ runWriter $ advance spell enemyDamage healthLossPerTurn state)) availableSpells
    heapWithNewStatesInserted = foldl (flip Heap.insert) heapWithLowestRemoved advancedStatesAndDists

--

parseStats :: [[String]] -> (Int, Int)
parseStats [["Hit", "Points:", hp], ["Damage:", damage]]
  = (read hp, read damage)

parse = parseStats . map words . lines

main = do
  input <- readFile "input22.txt"
  let (enemyHp, enemyDamage) = parse input
  let startingState = Ongoing 500 50 enemyHp (0, 0, 0)
  putStrLn $ show $ winSpendingLeastMana startingState enemyDamage 0
  let startingStateAfterInitialLoss = Ongoing 500 49 enemyHp (0, 0, 0)
  putStrLn $ show $ winSpendingLeastMana startingStateAfterInitialLoss enemyDamage 1

battle1 = putStr $ unlines $ snd $ runWriter $ tell ["-- Player turn --"] >> return (Ongoing 250 10 13 (0, 0, 0)) >>= displayStatus >>= advance Poison 8 0 >>= advance MagicMissile 8 0
battle2 = putStr $ unlines $ snd $ runWriter $ tell ["-- Player turn --"] >> return (Ongoing 250 10 14 (0, 0, 0)) >>= displayStatus >>= advance Recharge 8 0 >>= advance Shield 8 0 >>= advance Drain 8 0 >>= advance Poison 8 0 >>= advance MagicMissile 8 0
