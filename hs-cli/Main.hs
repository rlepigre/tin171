module Main
       where

import Network
import System.Environment( getArgs )
import System.Exit( exitSuccess )
import System.Random( randomRIO )
import Data.List
import Data.BERT.Parser
import Data.BERT
import Data.BERT.Term
import Data.BERT.Types
import System.IO
import System.Timeout
import Data.Char(isDigit)
import Control.Exception
import Control.Applicative((<$>))
import Control.Concurrent(threadDelay)
import Control.Monad(foldM)
import Data.Maybe

import Board hiding (Player, move)
data Player = Player { identity :: Int
                     , pName    :: String
                     } deriving (Eq, Show)
data Game = Game { gName   :: String
                 , players :: [Player]
                 , num     :: Int
                 } deriving (Eq, Show)

data MoveStatus = OK | Won | Err String
                deriving (Eq, Show)

newPlayer playerName = Player 0 playerName

addPlayers ps game = Game { gName   = gName game
                               , players = ps ++ players game
                               , num = num game + 1
                               }
removePlayer player game = let newPlayers = removePlayer' player (players game)
                            in Game { gName = gName game
                                    , players = newPlayers
                                    , num = num game - 1
                                    }

removePlayer' player [] = []
removePlayer' player (p:ps) | player == p =  ps
                            | otherwise  =  p : removePlayer' player ps

newGame gameName ps = let pls = map (\x -> newPlayer x) ps
                      in Game { gName = gameName
                              , players = pls
                              , num = length pls
                              }

main :: IO ()
main = do
  args <- getArgs
  case args of
    [hostName, portStr, userName, mode] -> do
      let port = PortNumber $ fromIntegral (read portStr)
      h <- connectTo hostName port
      hSetBuffering h LineBuffering
      login h userName
      putStrLn $ "Logged in as " ++ userName
      lobby h userName mode

lobby h userName "host" = do
  gameNr <- randomRIO (1 :: Int, 10000)
  let gameName = userName ++ "-" ++ show gameNr
  hostGame h gameName
  putStrLn $ "Hosted game: " ++ gameName ++ "\n Waiting for players..."
  waitForPlayers h 2
  putStrLn "Players joined, starting game."
  startGame h
  putStrLn "Game started, playing game."
  playGame h
lobby h userName "join" = do
  (ngames,_) <- listGames h
  case ngames of
    []     -> do
      putStrLn "No games, retrying soon."
      threadDelay 1000000
      lobby h userName "join"
    gs -> do
      res <- joinAGame h gs
      case res of
        Just g  -> do
          putStrLn $ "Joined game: " ++ g
          playGame h
        Nothing -> do
          putStrLn $ "Could not join any game."

waitForPlayers h n = do
  pls <- recv h
  case pls of
    Right (TupleTerm [ AtomTerm "player_joined"
                     , playersTerm ]) ->
      let (Right players) = readBERT playersTerm :: Either String [String]
      in if length players == n
         then return ()
         else waitForPlayers h n
    _ -> waitForPlayers h n

startGame h = call h startGameTerm

login h userName = call h (loginTerm userName)

playGame h = do
  pid <- gameStart h
  gameLoop h (fromIntegral pid)

gameLoop h pid = do
  res <- recv h
  putStrLn $ "Received: " ++ show res
  case res of
    Right (TupleTerm [ AtomTerm "won"
                     , whoWon
                     , boardTerm
                     ]) -> let Right winner = readBERT whoWon :: Either String (Int, String)
                           in putStrLn $ "Winner: " ++ show winner
    Right (TupleTerm [ AtomTerm "your_turn"
                     , timeoutTerm
                     , boardTerm
                     ]) -> do
      let (Right boardStr) = readBERT boardTerm :: Either String String
          moves@((cost, (_,_,bestMove)):_) = findMove pid boardStr
      putStrLn $ "Moves: " ++ show moves
      putStrLn $ "My turn. Move: " ++ show bestMove ++ " with cost: " ++ show cost
      status <- move h bestMove
      case status of
        Won -> putStrLn "I won."
        OK -> do
          putStrLn "Move ok."
          gameLoop h pid
        Err str -> putStrLn str
    Right term -> do
      putStrLn $ show term
      gameLoop h pid
    Left err -> do
      putStrLn $ show err
      gameLoop h pid

move h m = do
  send h $ moveTerm m
  move' h m

move' h m = do
  res <- recv h
  putStrLn $ show res
  case res of
    Right (AtomTerm "ok") -> return OK
    Right (TupleTerm [ AtomTerm "won"
                     , wonTerm
                     , boardTerm]) -> return Won
    Right term -> move' h m
    Left err -> return . Err $ "Errorparse: " ++ show err

gameStart h = do
  res <- recv h
  case res of
    Right (TupleTerm [ AtomTerm "game_start"
                     , idTerm
                     , playersTerm
                     , boardTerm
                     ]) ->
      let (Right id) = readBERT idTerm :: Either String Int
      in return id
    Right term -> error $ "Error: Expected game_start got: " ++ show term
    Left err -> error $ "Error: " ++ show err

joinAGame :: Handle -> [String] -> IO (Maybe String)
joinAGame h [] = return Nothing
joinAGame h (g:gs) = do
  res <- joinGame h g
  case res of
    True  -> return $ Just g
    False -> joinAGame h gs

joinGame :: Handle -> String -> IO Bool
joinGame h g = do
  send h $ joinGameTerm g
  resp <- recv h
  case resp of
    Right (AtomTerm "ok") -> return True
    _ -> return False

loginTerm :: String -> Term
loginTerm userName = TupleTerm [ AtomTerm "login"
                               , showBERT userName
                               ]

hostGame h gameName = call h $ hostGameTerm gameName

hostLoop = undefined

listGames h = do
  send h listGamesTerm
  games <- recv h
  case games of
    Right (TupleTerm [ AtomTerm "games"
                     , newGames
                     , runGames ]) ->
      let (Right xs) = readBERT newGames :: Either String [String]
          (Right ys) = readBERT runGames :: Either String [String]
      in return (xs,ys)
    Right term -> error $ show term
    Left err -> error $ show err

call h term = do
  send h term
  st <- recv h
  case st of
    Right (AtomTerm "ok") -> return ()
    Right term -> error $ show term
    Left err -> error $ show err

send h term = putStrLn ("Sent: " ++ show term) >> hPutStrLn h (show term ++ ".")

recv h = do
  str <- hGetLine h
  return $ parseTerm str

leaveTerm :: Term
leaveTerm = AtomTerm "leave"

listGamesTerm :: Term
listGamesTerm = AtomTerm "list_games"

listPlayersTerm :: Term
listPlayersTerm = AtomTerm "list_players"

startGameTerm :: Term
startGameTerm = AtomTerm "start_game"

spectateTerm :: String -> Term
spectateTerm gameName = TupleTerm [ AtomTerm "spectate"
                                  , showBERT gameName
                                  ]

moveTerm :: [Int] -> Term
moveTerm moves = TupleTerm [ AtomTerm "move"
                           , showBERT moves
                           ]

joinGameTerm :: String -> Term
joinGameTerm gameName = TupleTerm [ AtomTerm "join_game"
                                  , showBERT gameName
                                  ]

hostGameTerm :: String -> Term
hostGameTerm gameName = TupleTerm [ AtomTerm "host_game"
                                  , showBERT gameName
                                  ]

leaveGame h _ = do
  call h leaveTerm
  exitSuccess