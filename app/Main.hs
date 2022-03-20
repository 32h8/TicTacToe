{-# LANGUAGE ScopedTypeVariables #-}
module Main where

-- import Lib

import Control.Monad.State.Lazy
import Control.Monad.Except

import qualified Data.List as L
import Text.Read (readMaybe)
import Data.Maybe ( isJust, listToMaybe, mapMaybe )

type Mark = Char
type Board = [[Mark]]
type Pos = (Int, Int)

type Game a = ExceptT String (StateT GameState IO) a

data GameState = MkGameState
    { moves :: Int
    , board :: Board
    }

initialState :: GameState
initialState = MkGameState
    { moves = 0
    , board = emptyBoard
    }

emptyBoard :: Board
emptyBoard = replicate 3 $ replicate 3 emptyMark

emptyMark :: Mark
emptyMark = '_'

playerMark :: Mark
playerMark = 'X'

computerMark :: Mark
computerMark = 'O'

incMoves :: GameState -> GameState
incMoves s = s { moves = 1 + moves s }

strFromBoard :: Board -> String
strFromBoard b =
    (++)
    columnNumbers
    $ concat
    $ zipWith (\n r-> show n ++ ":" ++ r ++ "\n") [1..]
    $ map (L.intersperse ' ') b
    where
        columnNumbers :: String
        columnNumbers =
            (" :" ++)
            $ (++ "\n")
            $ unwords
            $ map show
            $ take (length $ head b) [1..]

mapBoard :: (Int -> Int -> Mark -> Mark) -> Board -> Board
mapBoard f b =
     zipWith (\r row -> zipWith (\c e -> f r c e) [1..] row) [1..] b

placeMark :: Mark -> Pos -> Board -> Board
placeMark xo (row, col) b =
    mapBoard f b
    where
        f r c e
            | r == row && c == col = xo
            | otherwise = e

indexedBoard :: Board -> [[(Pos, Mark)]]
indexedBoard b = zipWith (\r row -> zipWith (\c e -> ((r,c),e)) [1..] row) [1..] b

markAt :: Int -> Int -> Board -> Maybe Mark
markAt rIdx cIdx b =
    lookup (rIdx,cIdx)
    $ concat
    $ indexedBoard b

modifyBoard :: (Board -> Board) -> Game ()
modifyBoard f = do
    modify $ \s -> s { board = f $ board s }

printBoard :: Game ()
printBoard = do
    s <- get
    liftIO $ putStrLn $ strFromBoard $ board s

getCoords :: Game Pos
getCoords = do
    l <- liftIO $ getLine
    when (L.isPrefixOf "q" l) $ do
        liftIO $ putStrLn "Quiting."
        throwError "User quits."
    let coords :: [Int] = mapMaybe (readMaybe :: String -> Maybe Int) $ take 2 $ words l
    if length coords /= 2
    then do
        liftIO $ putStr "Invalid input. Try again: "
        getCoords
    else let [row,col] = coords in return (row,col)

getValidCoords :: Game Pos
getValidCoords = do
    coords@(row, col) <- getCoords
    maybe_mark <- fmap (markAt row col . board) get
    case maybe_mark of
        Nothing -> do
            liftIO $ putStr "No such position. Try again: "
            getValidCoords
        (Just e) ->
            if e /= '_'
            then do
                liftIO $ putStr "Position not avaliable. Try again: "
                getValidCoords
            else return coords

shouldEndGame :: Board -> Bool
shouldEndGame b =
    filledRow
    || filledCol
    || filledDiagonal
    || boardFilled b
    where
        filledRow = any equalNotEmpty b
        filledCol = any equalNotEmpty $ L.transpose b
        filledDiagonal =
            equalNotEmpty (diagonal1 b)
            || equalNotEmpty (diagonal2 b)

equalNotEmpty :: [Mark] -> Bool
equalNotEmpty [] = True
equalNotEmpty (x:xs) = all (== x) xs && x /= emptyMark

diagonal1 :: [[a]] -> [a]
diagonal1 [[x,_,_],[_,y,_],[_,_,z]] = [x,y,z]
diagonal2 :: [[a]] -> [a]
diagonal2 [[_,_,x],[_,y,_],[z,_,_]] = [x,y,z]

boardFilled :: Board -> Bool
boardFilled b = notElem emptyMark $ concat b

playerTurn :: Game ()
playerTurn = do
    liftIO $ putStrLn $ "Your turn (" ++ [playerMark] ++ ")."
    liftIO $ putStrLn "Type q to quit or"
    liftIO $ putStr "Type two space separated integers - row and col indexes: "
    coords <- getValidCoords
    modifyBoard $ placeMark playerMark coords
    liftIO $ putStrLn "Placed mark."
    modify incMoves

computerTurn :: Game ()
computerTurn = do
    liftIO $ putStrLn $ "Computer turn (" ++ [computerMark] ++ ")."
    liftIO $ putStrLn "thinking..."
    b <- fmap board get
    let p@(row, col) = case sureWinOrDraw computerMark b of
            (Nothing, Nothing) -> head $ allEmptyPos b
            (Just p, _) -> p
            (Nothing, Just p) -> p
    modifyBoard $ placeMark computerMark p
    liftIO $ seq col $ seq row $ putStrLn $ "Placed mark at " ++ show row ++ " " ++ show col ++ "."

oppositeMark :: Mark -> Mark
oppositeMark x
    | x == playerMark = computerMark
    | x == computerMark = playerMark
    | otherwise = error "invalid mark"

-- returns pair
--  first elem is move to win
--  second elem is move to tie
sureWinOrDraw :: Mark -> Board -> (Maybe Pos, Maybe Pos)
sureWinOrDraw mark b
    | 1 == length positions =
        let p = head positions
            b2 = placeMark mark p b
        in if won mark b2
            then (Just p, Nothing)
            else if won (oppositeMark mark) b2
                then (Nothing, Nothing)
                else (Nothing, Just p)
 
    | 1 < length positions = 
        let l = filter (\(x,y) -> isJust x || isJust y)
                $ flip map positions
                $ \p -> 
                    let b2 = placeMark mark p b
                    in if won mark b2
                        then (Just p, Nothing)
                        else case sureWinOrDraw (oppositeMark mark) b2 of
                            (Nothing, Nothing) -> (Just p, Nothing)
                            (Nothing, Just _) -> (Nothing, Just p)
                            (Just _, _) -> (Nothing, Nothing)
        in case listToMaybe $ filter (isJust . fst) l ++ filter (isJust . snd) l of
            Nothing -> (Nothing, Nothing)
            (Just z) -> z 

    | otherwise = error "board is filled" -- TODO maybe return pair (Nothing, Nothing) ?
    where 
        positions = allEmptyPos b

allEmptyPos :: Board -> [Pos]
allEmptyPos b = map fst $ filter ((== emptyMark) . snd) $ concat $ indexedBoard b

won :: Mark -> Board -> Bool
won mark b =
    filledRow
    || filledCol
    || filledDiagonal
    where
        filledRow = any (\xs -> equalNotEmpty xs && head xs == mark) b
        filledCol = any (\xs -> equalNotEmpty xs && head xs == mark) $ L.transpose b
        filledDiagonal =
            (\xs -> equalNotEmpty xs && head xs == mark) (diagonal1 b)
            || (\xs -> equalNotEmpty xs && head xs == mark) (diagonal2 b)

computerWon :: Board -> Bool
computerWon = won computerMark

playerWon :: Board -> Bool
playerWon = won playerMark

data GameResult = PlayerWon | ComputerWon | Draw | Incomplete deriving (Eq, Show)

resultFromBoard :: Board -> GameResult
resultFromBoard b
    | playerWon b = PlayerWon
    | computerWon b = ComputerWon
    | boardFilled b = Draw
    | otherwise = Incomplete

game :: Game ()
game = do
    end <- turn playerTurn
    when end printBoard
    unless end $ do
        end <- turn computerTurn
        when end printBoard
        unless end game
    where
        turn :: Game () -> Game Bool
        turn t = do
            printBoard
            filled <- fmap (boardFilled . board) get
            if filled
            then return True
            else do
                t
                fmap (shouldEndGame . board) get

main :: IO ()
main = do
    (_, s) <- runStateT (runExceptT game) initialState 
    let result = resultFromBoard $ board s
    let movesCount = moves s
    putStrLn $ "moves count: " ++ show movesCount
    case result of
        PlayerWon -> putStrLn "Player won."
        ComputerWon -> putStrLn "Computer won."
        Draw -> putStrLn "Draw."
        Incomplete -> putStrLn "Game aborted."
