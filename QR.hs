import qualified Data.Map as M
import Data.Maybe (isJust, fromJust)
import Data.List (sortBy, foldl', intercalate)
import Data.Function (on)
import Data.Ord (comparing)
import Control.Arrow ((&&&), first, second)
import Control.Monad (liftM)
import qualified Data.Foldable as F

import Debug.Trace
import Data.IORef
import System.IO.Unsafe

import Satchmo.Solver.Minisat
import Satchmo.Boolean hiding (not)
import Satchmo.Code

import SAT.Util

data Constraint = Nbrs Int | Any
                  deriving (Eq, Show)

data Cell = Filled | Empty | Unknown
            deriving (Eq, Show)

boardSize = 21

newtype Coord = C (Int, Int)
    deriving (Eq, Ord, Show)
type Board x = M.Map Coord x

type Problem = Board Constraint

inBounds :: Coord -> Bool
inBounds (C (x,y)) = x >=0 && x < boardSize && y >= 0 && y < boardSize

allCoords :: [Coord]
allCoords = do x <- [0..boardSize-1]
               y <- [0..boardSize-1]
               return $ C (x,y)
showBoard :: Board b -> (Maybe b -> Char) -> String
showBoard w square = unlines rows
    where rows = [row i | i <- [0..boardSize-1]]
          row r = [square $ M.lookup (C (r,c)) w | c <- [0..boardSize-1]]

nbrs :: Coord -> [Coord]
nbrs (C (x,y)) = do [C (x+dx,y+dy) | (dx,dy) <- deltas]

deltas :: [(Int,Int)]
deltas = [(x,y) | x <- [-1..1], y <- [-1..1]]

--

toSAT :: Problem -> SAT (Decoder (Board Bool))
toSAT p = do board <- liftM M.fromList $ sequence $ zipWith (\c -> liftM ((,) c)) allCoords (repeat boolean)
             mapM (assertOne board) $ allCoords
             return $ decode board
    where assertOne b c =
              case (p M.! c) of
                Any    -> return ()
                Nbrs n -> exactlyNof n $ map (b M.!) $ filter inBounds $ nbrs c

--
template = [ "   3  331   33 4  34 "
           , " 6 6 6   566  4    64"
           , "  4 4 3 4  4 435  4 3"
           , "36      5  33  66   3"
           , "35 6453 4 5  535 645 "
           , "465   4535 64   5    "
           , "  3 34 3   7 53  33 3"
           , "4   4  435 765 4 6 5 "
           , "2  5  13  7 7 3  3 4 "
           , " 56    5 9 8 6 5443 4"
           , "2 6  52 5 7  6 5   44"
           , "   5   5   5  6 222  "
           , "  44  3 5 6 45 4  2 3"
           , "2 4 4  4 6  5 4 333 2"
           , " 43 3 33 5 5 3222 23 "
           , "4 5 56 3 4 54  4 43 3"
           , "  4  5 4  6 233 3 4 4"
           , "3  9    3  5  5744  3"
           , "  46 536464 3  4  3 1"
           , " 65   4   3  4433332 "
           , "     4 4 532 2     11"]

qrprob = M.fromList $ concat rows where
    rows = [row r line | (r, line) <- zip [0..] template]
    row r line = [(C (r,c), square char) | (c,char) <- zip [0..] line]
    square ' ' = Any
    square i   = Nbrs $ read $ i:[]

main :: IO ()
main = do solved <- liftM fromJust $ solve $ toSAT qrprob
          putStrLn $ showBoard solved showOne
    where showOne Nothing      = '?'
          showOne (Just True)  = '#'
          showOne (Just False) = ' '
