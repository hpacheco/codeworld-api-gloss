{-# LANGUAGE CPP #-}

module Graphics.Gloss.Interface.Environment (getScreenSize,getDisplay) where

import Graphics.Gloss.Data.Display

import CodeWorld as CW

#if defined(ghcjs_HOST_OS)
    
import qualified GHCJS.DOM.DOMRect as DOMRect

#else
    


#endif

#if defined(ghcjs_HOST_OS)
    
orError str m = m >>= \x -> case x of
    Nothing -> Prelude.error $ str
    Just x -> return x

getSizeOf :: String -> IO (Double,Double)
getSizeOf iden = do
    doc <- orError ("getSizeOf doc " ++ show iden) currentDocument
    canvas <- orError ("getSizeOf iden " ++ show iden) $ getElementById doc (fromString iden :: JSString)
    rect <- getBoundingClientRect canvas
    cx <- DOMRect.getWidth rect
    cy <- DOMRect.getHeight rect
    return (cx,cy)
    
getScreenSize :: IO (Int, Int)
getScreenSize = do
    (x,y) <- getSizeOf "screen"
    return (floor x,floor y)
    
#else

getScreenSize :: IO (Int, Int)
getScreenSize = error $ "canvas screen size unknown" 

#endif

getDisplay :: IO Display
getDisplay = do
    (x,y) <- getScreenSize
    return $ Display x y