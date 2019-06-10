{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Game where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Event

play :: Display -> Color -> Int -> world -> (world -> Picture) -> (Event -> world -> world) -> (Float -> world -> world) -> IO ()
play display back framerate start draw react step = CW.interactionOf start stepCW reactCW drawCW
    where
    stepCW f w = step (realToFrac f) w
    reactCW e w = react (eventFromCW display e) w
    drawCW w = displayCWPicture display back (draw w)

playFitScreen :: Display -> Display -> Color -> Int -> world -> (world -> Picture) -> (Event -> world -> world) -> (Float -> world -> world) -> IO ()
playFitScreen screen display back framerate start draw react step = CW.interactionOf start stepCW reactCW drawCW
    where
    stepCW f w = step (realToFrac f) w
    reactCW e w = case fitScreenEvent screen display (eventFromCW screen e) of
        Nothing -> w
        Just e' -> react e' w
    drawCW w = displayCWPicture screen back (fitScreenPicture screen display $ draw w)



