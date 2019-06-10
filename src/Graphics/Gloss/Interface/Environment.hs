module Graphics.Gloss.Interface.Environment where

import CodeWorld as CW

getScreenSize :: IO (Int, Int)
getScreenSize = do
    (x,y) <- CW.getSizeOf "screen"
    return (floor x,floor y)