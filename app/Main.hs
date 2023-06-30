{-# LANGUAGE InstanceSigs #-}

module Main where

import           Control.Monad             (zipWithM_)
import           Control.Monad.Trans.Accum (AccumT, accum, add, execAccumT,
                                            runAccumT)
import           Data.Bits                 (xor)
import           Text.Printf               (printf)

newtype Board =
  Board [Int]

instance Semigroup Board where
  (<>) :: Board -> Board -> Board
  (<>) (Board xs) (Board ys) = Board (zipWith (+) xs ys)

instance Monoid Board where
  mempty :: Board
  mempty = Board (repeat 0)

type Move = (Int, Int)

nimSum :: Board -> Int
nimSum (Board rows) = foldl xor 0 rows

putBoard :: Board -> IO ()
putBoard b@(Board rows) = do
  zipWithM_ fmt [1 ..] rows
  let s = nimSum b
  printf "\x3A3 : %04b\n" s -- '\x3A3' = uppercase sigma
  where
    fmt :: Int -> Int -> IO ()
    fmt k n = printf "%d : %04b %s\n" k n $ concat $ replicate n "* "

applyMove :: Move -> AccumT Board IO ()
applyMove (k, m) = add $ Board $ replicate k 0 ++ [-m] ++ repeat 0

main :: IO ()
main = do
  let moves = [(2, 1), (1, 2), (0, 3)]
  board <- execAccumT (mapM_ applyMove moves) $ Board [5,4 .. 1]
  putBoard board
