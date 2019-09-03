{-# LANGUAGE Trustworthy #-}

module Graphics.Gloss.Data.Display where

import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color

import qualified CodeWorld as CW
--import qualified CodeWorld.Picture as CW
--import qualified CodeWorld.Driver as CW

data Display = Display Int Int

-- | Convert pictures with coordinates x in [-screenx/2,screenx/2], y in [-screeny/2,screeny/2] to codeworld pictures in the x,y in [-10,10] plane.
fitCWPictureToScreen :: Display -> CW.Picture -> CW.Picture
fitCWPictureToScreen (Display cx cy) p = CW.scaled (10 * factorx / cx2) (10 * factory / cy2) p
    where
    rcx = realToFrac cx
    rcy = realToFrac cy
    cx2 = rcx / 2
    cy2 = rcy / 2
    factorx = max 1 (rcx / rcy)
    factory = max 1 (rcy / rcx)
    

displayCWPicture :: Display -> Picture -> CW.Picture
displayCWPicture screen@(Display cx cy) p = fitCWPictureToScreen screen (pictureToCW p)
    where
    cx2 = realToFrac cx / 2
    cy2 = realToFrac cy / 2

-- fits a picture drawn for a display into a screen
fitScreenPicture :: Display -> Display -> Picture -> Picture
fitScreenPicture screen@(Display sx sy) display@(Display cx cy) pic = Scale scalexy scalexy pic
    where
    scalex = realToFrac sx / realToFrac cx
    scaley = realToFrac sy / realToFrac cy
    scalexy = min scalex scaley


