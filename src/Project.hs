{-# LANGUAGE OverloadedStrings #-}
module Project where

import           CodeWorld

myPicture :: Picture
myPicture = colored blue (lettering "Hello, world!")

run :: IO ()
run = drawingOf myPicture
