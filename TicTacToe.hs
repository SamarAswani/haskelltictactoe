module TicTacToe where

import Data.Char
import Data.Maybe
import Data.List
import Text.Read

-------------------------------------------------------------------
data Player = O | X
            deriving (Eq, Show)

data Cell = Empty | Taken Player
          deriving (Eq, Show)

type Board = ([Cell], Int)

type Position = (Int, Int)

-------------------------------------------------------------------

--
-- Some useful functions from, or based on, the unassessed problem sheets...
--

-- Preserves Just x iff x satisfies the given predicate. In all other cases
-- (including Nothing) it returns Nothing.
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe p m@(Just x)
  | p x = m
filterMaybe p _
  = Nothing

-- Replace nth element of a list with a given item.
replace :: Int -> a -> [a] -> [a]
replace 0 p (c : cs)
  = p : cs
replace _ p []
  = []
replace n p (c : cs)
  = c : replace (n - 1) p cs

-- Returns the rows of a given board.
rows :: Board -> [[Cell]]
rows (cs , n)
  = rows' cs
  where
    rows' []
      = []
    rows' cs
      = r : rows' rs
      where
        (r, rs) = splitAt n cs

-- Returns the columns of a given board.
cols :: Board -> [[Cell]]
cols
  = transpose . rows

-- Returns the diagonals of a given board.
diags :: Board -> [[Cell]]
diags (cs, n)
  = map (map (cs !!)) [[k * (n + 1) | k <- [0 .. n - 1]],
                      [k * (n - 1) | k <- [1 .. n]]]

-------------------------------------------------------------------

gameOver :: Board -> Bool
gameOver b
  | True `elem` ([ nub x == [Taken X] || nub x == [Taken O] |x<- rows b] ++ [ nub x == [Taken X] || nub x == [Taken O]|x<- cols b] ++ [ nub x == [Taken X] || nub x == [Taken O] |x<- diags b]) = True
  | otherwise = False  

noWin :: Board -> Bool
noWin (b,n) = not (Empty `elem` nub b)   

-------------------------------------------------------------------

--
-- Moves must be of the form "row col" where row and col are integers
-- separated by whitespace. Bounds checking happens in tryMove, not here.
--
parsePosition :: String -> Maybe Position
parsePosition "" = Nothing
parsePosition str
  | length(words str) > 2        = Nothing 
  | x == Nothing || y == Nothing = Nothing
  | otherwise                    = Just (fromJust(x),fromJust(y))
    where
      x = readMaybe ((words str) !! 0) :: Maybe Int
      y = readMaybe ((words str) !! 1) :: Maybe Int


tryMove :: Player -> Position -> Board -> Maybe Board
tryMove p (i,j) (b, n)
  | ((i * n) + j) < 0 || ((i * n) + j) >= length b = Nothing  
  | (b !! ((i * n) + j)) == Taken O || (b !! ((i * n) + j)) == Taken X  = Nothing
  | otherwise = Just (replace ((i * n) + j) (Taken p) b, n)    


-------------------------------------------------------------------
-- I/O Functions


-- prettyPrint :: Board -> IO ()
-- prettyPrint (iBoard, n)
--   = putStrLn (index (construct iBoard) 1 n)
--     where 
--       index :: String -> Int -> Int -> String
--       index [] _ _ = []
--       index (c:cs) i no
--         | i `mod` no == 0 = [c] ++ "\n" ++ index cs (i+1) no
--         | otherwise       = [c] ++ " " ++ index cs (i+1) no

--       construct :: [Cell] -> String
--       construct (Empty:bs)  = "-" ++ construct bs
--       construct ((Taken O):bs) = "O" ++ construct bs
--       construct ((Taken X):bs) = "X" ++ construct bs
--       construct [] = []

prettyPrint :: Board -> IO()
prettyPrint b
  = putStrLn printBoard
    where
      br = map (map (\a -> if a == Empty then "-" else let Taken a' = a in show a')) (rows b)
      brSpace = map (\a -> intersperse " " a) br
      printBoard = concat (intersperse "\n" (map concat brSpace))



-- The following reflect the suggested structure, but you can manage the game
-- in any way you see fit.

-- Repeatedly read a target board position and invoke tryMove until
-- the move is successful (Just ...).
takeTurn :: Board -> Player -> IO Board
takeTurn b p 
  = do 
      let player= show p
      putStr ("\nPlayer " ++ player ++ ", please enter a position: " )
      input <- getLine
      let position = parsePosition input
      case position of 
        Nothing -> do 
          putStrLn ("Invalid position form, please try again.")
          takeTurn b p
        Just _ -> do 
          let move = tryMove p (fromJust position) b
          case move of 
            Nothing -> do
              putStrLn ("Invalid position on board or occupied, please try again.")
              takeTurn b p
            Just _ -> do 
              let moveState = fromJust move
              return moveState

getSize :: IO Int
getSize
  = do 
      userInput <- getLine
      let size = readMaybe userInput :: Maybe Int
      case size of
        Nothing -> do 
          putStr "Please try again, input a single number: "
          getSize
        Just _ -> do
          if fromJust size <1
            then do 
              putStr "Please try again, input a number greater than 0: "
              getSize
          else
            return (fromJust size)        

-- Manage a game by repeatedly: 1. printing the current board, 2. using
-- takeTurn to return a modified board, 3. checking if the game is over,
-- printing the board and a suitable congratulatory message to the winner
-- if so.
playGame :: Board -> Player -> IO ()
playGame b p
  = do prettyPrint b
       newBoard <- takeTurn b p
       if (gameOver newBoard) then putStrLn ("Game Over, Player: " ++ show p ++ " wins.")
       else
         if (noWin newBoard) then putStrLn ("No player wins.")
         else
          if p == O then 
            playGame newBoard X
          else
            playGame newBoard O
                    

-- Print a welcome message, read the board dimension, invoke playGame and
-- exit with a suitable message.
main :: IO ()
main 
  = do putStrLn "Welcome to tic tac toe on an N x N board \nEnter the board size (N):"
       size <- getSize
       let board = (replicate (size^2) Empty, size)
       playGame board X
       return () 


-- doParseAction :: (String -> Maybe a) -> IO a
-- doParseAction Nothing 
--   = do putStrLn "Invalid input, try again: "
--        input <- getLine 
--        doParseAction readMaybe input :: IO Int
-- doParseAction Just n = return n 

-- doParseAction :: (String -> Maybe a) -> IO a
-- doParseAction readMaybe input ::   
--   = do putStrLn "Invalid input, try again: "
--        input <- getLine 
--        doParseAction readMaybe input :: IO Int
-- doParseAction Just n = return n     

-------------------------------------------------------------------

testBoard1, testBoard2, testBoard3 :: Board

testBoard1
  = ([Taken O,Taken X,Empty,Taken O,
      Taken O,Empty,Taken X,Taken X,
      Taken O,Empty,Empty,Taken X,
      Taken O,Taken X,Empty,Empty],
      4)

testBoard2
  = ([Taken X,Empty,
      Empty,Empty],
      2)

testBoard3
  = ([Taken O,Taken X,Empty,Taken O,Taken X,
      Taken O,Empty,Taken X,Taken X,Empty,
      Empty,Empty,Taken X,Taken O,Taken O,
      Taken O,Taken X,Empty,Empty,Taken X,
      Taken X,Empty,Taken O,Empty,Empty],
      5)
