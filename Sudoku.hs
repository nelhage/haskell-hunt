import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.Array.IArray
import Data.List (find, intercalate)
import Control.Monad (liftM)

import Satchmo.Solver.Minisat
import Satchmo.Boolean hiding (not)
import Satchmo.Code

import SAT.Util

boardSize  = 9
squareSize = (floor . sqrt) 9

newtype Coord = C (Int, Int)
    deriving (Eq, Ord, Show, Ix)

type Board x = Array Coord x

type Problem = Board (Maybe Int)

groups :: [[Coord]]
groups = rows ++ cols ++ squares
    where rows    = [[C(i, j) | j <- [0..boardSize-1]] | i <- [0..boardSize-1]]
          cols    = [[C(j, i) | j <- [0..boardSize-1]] | i <- [0..boardSize-1]]
          squares = [[C(i*squareSize+dx,j*squareSize+dy)
                      | dx <- [0..squareSize-1], dy <- [0..squareSize-1]]
                     | i <- [0..squareSize-1], j <- [0..squareSize-1]]

sudoku :: SAT (Board [Boolean])
sudoku = do bs <- sequence $ take (boardSize ^ 2) $ map sequence $ repeat (take boardSize $ repeat boolean)
            board <- return $ listArray (C (0, 0), C (boardSize - 1, boardSize - 1)) bs
            mapM (exactlyNof 1) $ elems board
            mapM doGroup $ map (map (board!)) $ groups
            return board
    where doGroup :: [[Boolean]] -> SAT ()
          doGroup grp = sequence_ [ exactlyNof 1 bs
                                    | i <- [0..boardSize-1], bs <- [map (!!i) grp] ]

puzzle :: Board (Maybe Int) -> SAT (Board [Boolean])
puzzle p = do b <- sudoku
              mapM (square b) $ assocs p
              return b
    where square _ (_, Nothing) = return ()
          square b (c, Just i)  = assert [ (b ! c) !! (i-1) ]

extract :: Board [Bool] -> Board Int
extract = amap toInt
    where toInt = fst . fromJust . find snd . zip [1..]

showSudoku :: Board Int -> String
showSudoku b = intercalate "\n" $ map line [0..boardSize-1]
    where line i = intercalate " " $ [show $ b ! C (i,j) | j <- [0..boardSize-1]]

parseSudoku :: String -> Board (Maybe Int)
parseSudoku s = array (C(0,0), C(boardSize-1,boardSize-1)) $ cells
    where cells  = zip coords $ map parse s
          coords = [C (r,c) | r <- [0..boardSize-1], c <- [0..boardSize-1]]
          parse ' ' = Nothing
          parse '.' = Nothing
          parse  i  = Just $ read (i:[])

b = concat $
    [ "   1 5   "
    , "14    67 "
    , " 8   24  "
    , " 63 7  1 "
    , "9       3"
    , " 1  9 52 "
    , "  72   8 "
    , " 26    35"
    , "   4 9   " ]

maybeGetLine :: IO (Maybe String)
maybeGetLine = catch (liftM Just getLine) (\_ -> return Nothing)

main :: IO ()
main = do loop
    where loop =
              do l <- maybeGetLine
                 case l of
                   Nothing   -> return ()
                   Just line ->
                       do b   <- return $ parseSudoku line
                          res <- solve $ liftM decode $ puzzle b
                          case res of
                            Nothing -> putStrLn "No solution"
                            Just s  -> putStrLn $ showSudoku $ extract s
                          putStrLn ""
                          loop
