{-# LANGUAGE OverloadedStrings #-}
module Project where

import CodeWorld

tree :: Int -> Picture
tree 0 = blank
tree n = trunk <> translated 0 1 (leftBranch <> rightBranch)
  where
    branch = tree (n - 1)
    theta = pi/9
    leftBranch = rotated theta branch
    rightBranch = rotated (-theta) branch
    trunk = translated 0 0.5 (solidRectangle 0.1 1)

ourTree :: Picture
ourTree = tree 10

run :: IO ()
run = drawingOf ourTree