
import System.IO (readFile)
import Control.Monad.Trans.Writer.Lazy (Writer, tell, runWriter)
import Data.Maybe (maybe, isJust)
import Control.Monad (guard)

--

data Weapon = Dagger | Shortsword | Warhammer | Longsword | Greataxe deriving Show

wCost :: Weapon -> Int
wCost Dagger = 8
wCost Shortsword = 10
wCost Warhammer = 25
wCost Longsword = 40
wCost Greataxe = 74

wDamage :: Weapon -> Int
wDamage Dagger = 4
wDamage Shortsword = 5
wDamage Warhammer = 6
wDamage Longsword = 7
wDamage Greataxe = 8

wArmor :: Weapon -> Int
wArmor _ = 0

data Armor = Leather | Chainmail | Splintmail | Bandedmail | Platemail deriving Show

aCost :: Armor -> Int
aCost Leather = 13
aCost Chainmail = 31
aCost Splintmail = 53
aCost Bandedmail = 75
aCost Platemail = 102

aDamage :: Armor -> Int
aDamage _ = 0

aArmor :: Armor -> Int
aArmor Leather = 1
aArmor Chainmail = 2
aArmor Splintmail = 3
aArmor Bandedmail = 4
aArmor Platemail = 5

data Ring = Damage Int | Defense Int deriving (Show, Eq)

rCost :: Ring -> Int
rCost (Damage 1) = 25
rCost (Damage 2) = 50
rCost (Damage 3) = 100
rCost (Defense 1) = 20
rCost (Defense 2) = 40
rCost (Defense 3) = 80

rDamage :: Ring -> Int
rDamage (Damage n) = n
rDamage (Defense _) = 0

rArmor :: Ring -> Int
rArmor (Damage _) = 0
rArmor (Defense n) = n

--

totalCost :: (Weapon, Maybe Armor, Maybe Ring, Maybe Ring) -> Int
totalCost (w, a, r1, r2) = wCost w + (maybe 0 aCost a) + (maybe 0 rCost r1) + (maybe 0 rCost r2)

totalDamage :: (Weapon, Maybe Armor, Maybe Ring, Maybe Ring) -> Int
totalDamage (w, a, r1, r2) = wDamage w + (maybe 0 aDamage a) + (maybe 0 rDamage r1) + (maybe 0 rDamage r2)

totalArmor :: (Weapon, Maybe Armor, Maybe Ring, Maybe Ring) -> Int
totalArmor (w, a, r1, r2) = wArmor w + (maybe 0 aArmor a) + (maybe 0 rArmor r1) + (maybe 0 rArmor r2)

allItemCombinations :: [(Weapon, Maybe Armor, Maybe Ring, Maybe Ring)]
allItemCombinations = do
  weapon <- [Dagger, Shortsword, Warhammer, Longsword, Greataxe]
  let optional items = Nothing : map Just items
  armor <- optional [Leather, Chainmail, Splintmail, Bandedmail, Platemail]
  let allRings = [Damage 1, Damage 2, Damage 3, Defense 1, Defense 1, Defense 3]
  leftHandRing <- optional allRings
  rightHandRing <- optional allRings
  guard $ not $ isJust leftHandRing && isJust rightHandRing && leftHandRing == rightHandRing
  return (weapon, armor, leftHandRing, rightHandRing)

--

cheapestWin :: (Int, Int, Int) -> Int
cheapestWin enemyStats = minimum $ map totalCost $ filter (desiredCompetitorWins Player enemyStats) allItemCombinations

mostExpensiveLoss :: (Int, Int, Int) -> Int
mostExpensiveLoss enemyStats = maximum $ map totalCost $ filter (desiredCompetitorWins Boss enemyStats) allItemCombinations

desiredCompetitorWins :: Competitor -> (Int, Int, Int) -> (Weapon, Maybe Armor, Maybe Ring, Maybe Ring) -> Bool
desiredCompetitorWins desiredWinner enemyStats items = winner == desiredWinner
  where
    playerStats = (100, totalDamage items, totalArmor items)
    battleResult = battle (Player, playerStats) (Boss, enemyStats)
    winner = fst $ runWriter battleResult

--

data Competitor = Player | Boss deriving Eq

instance Show Competitor where
  show Player = "player"
  show Boss = "boss"

battle :: (Competitor, (Int, Int, Int)) -> (Competitor, (Int, Int, Int)) -> Writer [String] Competitor
battle (competitorA, (hpA, damageA, armorA)) (competitorB, (hpB, damageB, armorB)) = do
  let damageDealt = max (damageA - armorB) 1
  let newHpB = hpB - damageDealt
  tell $ ["The " ++ show competitorA ++ " deals " ++ show damageA ++ "-" ++ show armorB ++ " = " ++ show damageDealt ++ " damage;"
      ++ " the " ++ show competitorB ++ " goes down to " ++ show newHpB ++ " hit points."]
  if newHpB <= 0 then
    return competitorA
  else
    battle (competitorB, (newHpB, damageB, armorB)) (competitorA, (hpA, damageA, armorA))

--

parseStats :: [[String]] -> (Int, Int, Int)
parseStats [["Hit", "Points:", hp], ["Damage:", damage], ["Armor:", armor]]
  = (read hp, read damage, read armor)

parse = parseStats . map words . lines

main = do
  input <- readFile "input21.txt"
  let enemyStats = parse input
  putStrLn $ show $ cheapestWin enemyStats
  putStrLn $ show $ mostExpensiveLoss enemyStats
