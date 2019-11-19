{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Simulate where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display

-- TODO: missing screen and background
simulate :: Display -> Color -> Int -> model -> (model -> Picture) -> (ViewPort -> Float -> model -> model) -> IO ()
simulate display back framerate state draw go = CW.simulationOf state goCW (colorToCW back) drawCW
    where
    goCW t w = go ViewPort (realToFrac t) w
    drawCW = displayCWPicture display . draw