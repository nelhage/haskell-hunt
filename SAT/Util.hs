{-# LANGUAGE MultiParamTypeClasses, UndecidableInstances #-}
module SAT.Util (atMostNof, atLeastNof, exactlyNof) where
import Prelude hiding (not)

import qualified Data.Map as M
import Control.Monad (liftM)

import Satchmo.Boolean
import Satchmo.Code

instance (Decode c a, Ord k) => (Decode (M.Map k c) (M.Map k a)) where
    decode m = liftM M.fromList $ mapM decodeone $ M.toList m
        where decodeone (k,v) = do v' <- decode v
                                   return $ (k, v')

choose :: [a] -> Int -> [[a]]
choose _  0 = [[]]
choose [] _ = []
choose (x:xs) n
    | n > (length xs + 1) = []
    | n > length xs       = [x:xs]
    | otherwise           = map (x:) (choose xs (n-1)) ++ (xs `choose` n)

atMostNof :: Int -> [Boolean] -> SAT ()
atMostNof k v = mapM_ (assert . map not) $ choose v (k+1)

atLeastNof :: Int -> [Boolean] -> SAT ()
atLeastNof k v = let tot = length v
                 in mapM_ assert $ choose v (tot-k+1)

exactlyNof :: Int -> [Boolean] -> SAT ()
exactlyNof k v = do atLeastNof k v
                    atMostNof k v
