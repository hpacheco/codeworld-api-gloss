{-# LANGUAGE CPP #-}

module Graphics.Gloss.Interface.Environment where

import Graphics.Gloss.Data.Display

import CodeWorld as CW
import CodeWorld.Driver as CW

import qualified Data.Text as T

playAudioById = CW.playAudioById
say = CW.say
reportRuntimeMessage = CW.reportRuntimeMessage

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
    
trace :: String -> a -> a
trace str = CW.trace (T.pack str)