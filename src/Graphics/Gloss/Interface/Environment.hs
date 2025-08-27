{-# LANGUAGE CPP #-}

module Graphics.Gloss.Interface.Environment where

import Graphics.Gloss.Data.Display

import CodeWorld as CW
import CodeWorld.Driver as CW

import qualified Data.Text as T
import Data.String

playAudioById = CW.playAudioById
playAudioByIdWithLoop = CW.playAudioByIdWithLoop
pauseAudioById = CW.pauseAudioById
say = CW.say
reportRuntimeMessage = CW.reportRuntimeMessage
addMessage typ msg = CW.addMessage (fromString typ) (fromString msg)

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

getTextContent :: IO String
getTextContent = CW.getTextContent

textWidth :: String -> IO Float
textWidth = CW.textWidth
