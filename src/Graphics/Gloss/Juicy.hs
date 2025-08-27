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

-- * to simulate Gloss BitmapSection field. we read bitmaps using JuicyPixels though

data Rectangle = Rectangle	{ rectPos :: (Int, Int), rectSize :: (Int, Int)	}
    deriving (Eq,Ord,Show,Enum,Bounded)
data RowOrder = TopToBottom	| BottomToTop
    deriving (Eq,Ord,Show,Enum,Bounded)

bitmapSection :: RowOrder -> Rectangle -> DynamicImage -> IO (Maybe Picture)
bitmapSection order r img = do
    Just pic <- fromDynamicImage $ cropDynamicImage order r
    return pic

cropImage :: Pixel a => RowOrder -> Rectangle -> Image a -> Image a
cropImage TopToBottom = cropImageTopToBottom
cropImage BottomToTop = cropImageBottomToTop

cropImageTopToBottom :: Pixel a => Rectangle -> Image a -> Image a
cropImageTopToBottom (Rectangle (x,y) (width,height)) img =
  generateImage (\dx dy -> pixelAt img (clampX (x + dx)) (clampY (y + dy))) width height
  where
    clampX pos = max 0 (min (imageWidth img - 1) pos)
    clampY pos = max 0 (min (imageHeight img - 1) pos)
    
cropImageBottomToTop :: Pixel a => Rectangle -> Image a -> Image a
cropImageBottomToTop (Rectangle (x,y) (width,height)) img = cropImageTopToBottom (Rectangle (x,invertY y) (width,height)) img
    where
    invertY y = imageHeight img - y - height

cropDynamicImage :: RowOrder -> Rectangle -> DynamicImage -> DynamicImage
cropDynamicImage order rect dynImg =
  dynamicPixelMap (cropImage order rect) dynImg

    






