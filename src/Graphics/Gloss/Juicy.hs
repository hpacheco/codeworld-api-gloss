module Graphics.Gloss.Juicy where

import qualified Codec.Picture as Juicy
import qualified Codec.Picture.Types as Juicy

import Graphics.Gloss.Data.Picture

import qualified CodeWorld.CanvasM as CW
import qualified CodeWorld.Picture as CW
import qualified CodeWorld.Driver as CW

import Control.Monad

import Data.JSString
import GHCJS.DOM
import GHCJS.DOM.Document hiding (evaluate)
import GHCJS.DOM.Element

toDynamicImage :: Picture -> IO (Maybe Juicy.DynamicImage)
toDynamicImage (Image w h cwimg) = do
    img <- CW.img2Canvas w h cwimg >>= CW.toJuicyImage
    return $ Just $ Juicy.ImageRGBA8 img
toDynamicImage _ = return Nothing

fromDynamicImage :: Juicy.DynamicImage -> IO (Maybe Picture)
fromDynamicImage (Juicy.ImageRGBA8 img)  = liftM Just $ fromImageRGBA8 img
fromDynamicImage i = return Nothing

fromImageRGBA8 :: Juicy.Image Juicy.PixelRGBA8 -> IO Picture
fromImageRGBA8 img@(Juicy.Image w h _) = do
    cwcanvas <- CW.fromJuicyImage img
    return $ Image w h $ CW.CanvasImg cwcanvas
    






