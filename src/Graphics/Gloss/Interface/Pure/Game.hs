{-# LANGUAGE TupleSections, Trustworthy #-}

module Graphics.Gloss.Interface.Pure.Game
    ( module Graphics.Gloss.Interface.Pure.Game
    , module Graphics.Gloss.Data.Picture
    , module Graphics.Gloss.Data.Color
    , module Graphics.Gloss.Data.Display
    , module Graphics.Gloss.Data.Event

) where

import qualified CodeWorld as CW

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Event

play :: Display -> Color -> Int -> world -> (world -> Picture) -> (Event -> world -> world) -> (Float -> world -> world) -> IO ()
play display back framerate start draw react step = CW.interactionOf (display,start) stepCW reactCW (colorToCW back) drawCW
    where
    stepCW f (disp,w) = (disp,step (realToFrac f) w)
    reactCW e (disp,w) = 
        case eventFromCW disp e of
            Nothing -> (disp,w)
            Just e@(EventResize (x,y)) -> (Display x y,react e w)
            Just e -> (disp,react e w)
    drawCW (disp,w) = displayCWPicture disp (draw w)

playFitScreen :: Display -> Display -> Color -> Int -> world -> (world -> Picture) -> (Event -> world -> world) -> (Float -> world -> world) -> IO ()
playFitScreen screen display back framerate start draw react step = CW.interactionOf (screen,start) stepCW reactCW (colorToCW back) drawCW
    where
    stepCW f (disp,w) = (disp,step (realToFrac f) w)
    reactCW e (disp,w) = case (eventFromCW disp e >>= fitScreenEvent disp display) of
        Nothing -> (disp,w)
        Just e@(EventResize (x,y)) -> (Display x y,react e w)
        Just e -> (disp,react e w)
    drawCW (disp,w) = displayCWPicture disp (fitScreenPicture disp display $ draw w)



