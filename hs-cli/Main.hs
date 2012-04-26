module Main
       where

import Network
import System.Environment( getArgs )
import System.Exit( exitSuccess )
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
      lobbyLoop h 0 userName mode

lobbyLoop h n userName mode@"host" = do
  let gameName = userName ++ show n
  hostGame h gameName
  putStrLn $ "Hosted game: " ++ gameName ++ "\n Waiting for players..."
  waitForPlayers h 2
  putStrLn "Players joined, starting game."
  startGame h
  putStrLn "Game started, playing game."
  playGame h
  lobbyLoop h (n+1) userName mode
lobbyLoop h n userName mode@"join" = do
  (ngames,_) <- listGames h
  case ngames of
    []     -> do
      putStrLn "No games, retrying soon."
      threadDelay 1000000
      lobbyLoop h n userName mode
    gs -> do
      res <- joinAGame h gs
      case res of
        Just g  -> do
          putStrLn $ "Joined game: " ++ g
          playGame h
          lobbyLoop h (n+1) userName mode
        Nothing -> do
          putStrLn $ "Could not join any game."
          threadDelay 1000000
          lobbyLoop h n userName mode

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
    Right (TupleTerm [ AtomTerm "your_turn"
                     , timeoutTerm
                     , boardTerm
                     ]) -> do
      let (Right boardStr) = readBERT boardTerm :: Either String String
          moves = findMove pid boardStr
          (cost, (Move start stop bestMove)) = head moves
      putStrLn $ "Moves: " ++ show moves
      putStrLn $ "My turn. Move: " ++ show bestMove
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

{-
gameLobby :: Handle -> Player -> IO ()
gameLobby h player = do
  gameLobbyMenu
  c <- getChar
  if (isDigit c)
    then (lobbyCommand h c player)
    else (do errorPrint "Menu item has to be integer"
             gameLobby h player)

lobbyCommand h c player = case findCommand c of
  Nothing -> do
    errorPrint "Choose a integer in the list"
    gameLobby h player
  Just fun ->
    fun h player

gameLobbyMenu = let
  wel = "Choose a menu item:\n"
  prompt = "Item: "
  itemStr = unlines $ map (\(id, str, fun) -> show id ++ ". " ++ str) lobbyItems
  in putStr $ wel ++ itemStr ++ prompt

findCommand i = let ls = map (\(id,_,fun) -> (id, fun)) lobbyItems
                in lookup i ls

lobbyItems = [('1', "Host game", hostGame)
             ,('2', "Join game", joinGame)
             ,('3', "Spectate", spectateGame)
             ,('4', "List players", listPlayers)
             ,('5', "List games", listGames)
             ,('6', "Leave", leaveGame)
             ]

usage :: String
usage = "Usage: hs-cli hostname port username"

hostGame h player = do
  getLine
  putStrLn "Game name: "
  gameName <- getLine
  let game = newGame gameName [player]
  doCommand h (hostGameTerm gameName)
    (do putStrLn "Game hosted!\nWrite start to start the game or leave to leave"
        hosted h game) player

startGame h game = do
  res <- call h startGameTerm
  case res of
    Nothing ->
      play h game
    Just err -> do
      errorPrint err
      hosted h game

joinGame = undefined
spectateGame = undefined
listGames = undefined
listPlayers = undefined

hosted h game@(Game name pls n) = do
  ready <- hReady h
  case ready of
    False -> do
      term <- timeout 10000 (getLine)
      case term of
        Nothing ->
          hosted h game
        Just str ->
          case str of
            "start" -> startGame h game
            "leave" -> leaveGame h game
            _       -> do
              putStrLn "Write start to start the game or leave to leave."
              hosted h game
    True -> do
      res <- recv h
      case res of
        Left err -> do
          errorPrint $ show err
          hosted h game
        Right term ->
          case term of
            TupleTerm [ AtomTerm "player_joined", playerList] -> do
              let Right ps = readBERT playerList :: Either String [String]
                  ng = newGame name ps
              playerJoined ps
              hosted h newGame
            TupleTerm [ AtomTerm "player_left", playerList] -> do
              let Right ps = readBERT playerList :: Either String [String]
                  ng = newGame name ps
              playerLeft ps
              hosted h ng

playerLeft players = do
  putStrLn "Player left\nPlayers:"
  mapM_ (\x -> putStr (x ++ " ")) players
  putStrLn ""

playerJoined players = do
  putStrLn "Player joined\nPlayers:"
  mapM_ (\x -> putStr (x ++ " ")) players
  putStrLn ""

errorPrint err = putStrLn $ "Error: " ++ err

doCommand h term action player = do
  res <- call h term
  case res of
    Just err -> do
      errorPrint err
      gameLobby h player
    Nothing ->
      action


play = undefined


-}