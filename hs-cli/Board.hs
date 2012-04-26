module Board
       where

import Data.Array.IArray
import Data.Char
import Data.Word
import Data.List
import qualified Data.Set as S

-- Interpreted as:
-- 255 out of bounds
-- 0 empty
-- 1:6 player pegs
type Player = Word8
type Board = Array Int Player

data Move = Move { start :: Int
                 , stop  :: Int
                 , moves :: [Int]
                 } deriving (Show)

instance Eq Move where
  (Move a b _) == (Move c d _) = a == c && b == d

instance Ord Move where
  (Move a b m1) `compare` (Move c d m2) = (a `compare` c) `compare`
                                          (b `compare` d)

findMove :: Player -> String -> [(Double, Move)]
findMove player boardStr = let
  board = parseBoard boardStr
  in searchMove player board

searchMove :: Player -> Board -> [(Double, Move)]
searchMove player board = naiveDistance player board

naiveDistance :: Player -> Board -> [(Double, Move)]
naiveDistance player board =
  sortBy cmp $ map score (legalMoves player board)
  where cmp (d1, _) (d2, _) = d1 `compare` d2
        goal = tip (goalNest player)
        score m = (totalDistance player (move board m), m)

move :: Board -> Move -> Board
move board (Move x y _) =
  let peg = board ! x
  in board // [(x,0),(y,peg)]

totalDistance player board = let
  goal = S.toList $ nest (goalNest player)
  pPegs  = getPlayerPegs player board
  inGoal = pPegs `intersect` goal
  pPegs' = pPegs \\ inGoal
  target = goal \\ pPegs
  in sum [distance x t | x <- pPegs', t <- target]

miniMax = undefined

possiblePositions :: Player -> S.Set Int
possiblePositions player =
  S.unions [nest player, nest (goalNest player), middle]

nest :: Player -> S.Set Int
nest player =
  S.fromList $ case player of
    1 -> [4,21,22,38,39,40,55,56,57,58]
    2 -> [230,231,232,233,248,249,250,266,267,284]
    3 -> [68,69,70,71,86,87,88,104,105,122]
    4 -> [166,183,184,200,201,202,217,218,219,220]
    5 -> [77,78,79,80,95,96,97,113,114,131]
    6 -> [157,174,175,191,192,193,208,209,210,211]

newBoard :: Board
newBoard = listArray (0,288) $
           [ if x `S.member` wholeBoard
             then 0
             else 255 | x <- [0..288]]

staticDistance :: Player -> Board
staticDistance player = stDist 0 nextFun [tip player] newBoard
  where nextFun = case player of
          1 -> \i -> [i+17,i+18]
          2 -> \i -> [i-18,i-17]
          3 -> \i -> [i+1,i+18]
          4 -> \i -> [i-1,i-18]
          5 -> \i -> [i-1,i+17]
          6 -> \i -> [i-17,i+1]

stDist _ _ [] board = board
stDist i next xs board =
  let onBoard x = x >= 0 && x <= 288 && board ! x /= 255
      nextPegs = filter onBoard $
                 nub . concat $ map next xs
  in stDist (i+1) next nextPegs (board // (map (\x -> (x,i)) xs))

goalNest :: Player -> Player
goalNest player | player `elem` [1,3,5] = player+1
                | otherwise  = player-1

middle :: S.Set Int
middle = S.fromList . concat $
         [[72..76], [89..94], [106..112], [123..130], [140..148]
         ,[158..165] ,[176..182], [194..199], [212..216]]

wholeBoard :: S.Set Int
wholeBoard = S.unions [nest i | i <- [1..6]] `S.union` middle

parseBoard :: String -> Array Int Player
parseBoard = listArray (0,288) . map (fromIntegral . fix)
  where fix '#' = 255
        fix ' ' = 0
        fix x = ord x - ord '0'

legalMoves :: Player -> Board -> [Move]
legalMoves player board =
  let steps = legalSteps player board
      jumps = legalJumps player board
  in steps ++ jumps

legalSteps :: Player -> Board -> [Move]
legalSteps player board = filter legal possibleSteps
  where
    possibleSteps = [Move i s [i,s] | i <- getPlayerPegs player board
                                     , s <- (allowedSteps i)
                                     , s `S.member` possiblePositions player]
    legal (Move start stop _) = inRange stop && unOccupied stop
    inRange x = x >= 0 && x <= 288
    unOccupied x = (board ! x) == 0

legalJumps :: Player -> Board -> [Move]
legalJumps player board =
  let playerPegs = getPlayerPegs player board
      startMoves = S.fromList $ map (\i -> Move i i [i]) playerPegs
      jumps = legalJumps' board player startMoves S.empty
  in S.toList (jumps S.\\ startMoves)

legalJumps' :: Board -> Player -> S.Set Move -> S.Set Move -> S.Set Move
legalJumps' board player newJumps jumpAcc
  | S.null newJumps = S.map (\(Move start stop moves) ->
                          Move start stop (reverse moves)) jumpAcc
  | otherwise = let
    newPossible =
      S.map (\(Move start stop ms) ->
            let js = zip (allowedSteps stop) (allowedJumps stop)
                js' = filter (\(s,j) ->
                               let jInRange = j >= 0 && j <= 288
                                   allowed = j `S.member`
                                             possiblePositions player &&
                                             s /= start && j /= start
                                   pegS = (board ! s)
                                   pegJ = (board ! j)
                               in jInRange && allowed &&
                                  pegS /= 0 && pegJ == 0) js
            in map (\(s,j) -> Move start j (j:ms)) js'
          )  newJumps
    newJumps' =  newJumps `S.union` jumpAcc
    newPossible' =
      (S.unions $ map S.fromList (S.toList newPossible)) S.\\ newJumps'
    in legalJumps' board player newPossible' newJumps'

getPlayerPegs :: Player -> Board -> [Int]
getPlayerPegs player board =
  map fst . filter (\(i,e) -> e == player) $ assocs board

allowedSteps pos = [pos-18, pos-17, pos-1, pos+1, pos+17, pos+18]

allowedJumps pos = [pos-36, pos-34, pos-2, pos+2, pos+34, pos+36]

distance :: Int -> Int -> Double
distance xP yP =
  let coord p = (fromIntegral $ p - p `mod` 17, fromIntegral $ p `mod` 17)
      dist (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)
  in dist (coord xP) (coord yP)

tip :: Player -> Int
tip 1 = 4
tip 2 = 284
tip 3 = 68
tip 4 = 220
tip 5 = 80
tip 6 = 208

testStr = "####1################11###############111##############1111#########             #####            ######           #######          ########         ########          #######           ######            #####             #########2222##############222###############22################2####"