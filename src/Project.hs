{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld

-- |
-- >>> 2 + 2
-- 4
myPicture :: Picture
myPicture = colored blue (lettering "Hello, world!")

run :: IO ()
run = drawingOf myPicture
