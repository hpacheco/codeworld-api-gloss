{-# LANGUAGE CPP #-}

module Graphics.Gloss.Interface.Environment (getScreenSize,getDisplay) where

import Graphics.Gloss.Data.Display

import CodeWorld as CW
import CodeWorld.Driver as CW

#if defined(ghcjs_HOST_OS)

getScreenSize :: IO (Int, Int)
getScreenSize = do
    (x,y) <- CW.getSizeOf "screen"
    return (floor x,floor y)
    
#else

getScreenSize :: IO (Int, Int)
getScreenSize = error $ "canvas screen size unknown" 

#endif

getDisplay :: IO Display
getDisplay = do
    (x,y) <- getScreenSize
    return $ Display x y