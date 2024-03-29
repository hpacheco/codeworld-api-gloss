{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE JavaScriptFFI #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

{-
  Copyright 2019 The CodeWorld Authors. All rights reserved.
  Licensed under the Apache License, Version 2.0 (the "License");
  you may not use this file except in compliance with the License.
  You may obtain a copy of the License at
      http://www.apache.org/licenses/LICENSE-2.0
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS,
  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  See the License for the specific language governing permissions and
  limitations under the License.
-}

module CodeWorld.CanvasM where

import           Data.String
import CodeWorld.Picture
import GHCJS.DOM.HTMLImageElement
import GHCJS.Marshal

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans (MonadIO)
import Data.Text (Text, pack)
import qualified Data.Text as T

#ifdef ghcjs_HOST_OS

import Data.JSString.Text
import GHCJS.DOM
import GHCJS.DOM.Element
import GHCJS.DOM.NonElementParentNode
import GHCJS.Marshal.Pure
import GHCJS.Types
import qualified JavaScript.Web.Canvas as Canvas
import qualified JavaScript.Web.Canvas.Internal as Canvas

import JavaScript.Web.Canvas.ImageData as ImageData
import JavaScript.TypedArray
import JavaScript.TypedArray.Internal
import GHC.Word
import GHC.Types
import GHC.Exts

--import qualified GHCJS.DOM.CanvasRenderingContext2D as ImageData
--import qualified GHCJS.DOM.ImageData as ImageData
--import GHCJS.DOM.Types (Uint8ClampedArray(..))

import qualified Data.Vector.Generic as V
import qualified Codec.Picture.Types as Juicy

#else

import qualified Graphics.Blank as Canvas
import Graphics.Blank (Canvas)
import Text.Printf

#endif

class (Monad m, MonadIO m) => MonadCanvas m where
    type Image m

    save :: m ()
    restore :: m ()
    transform ::
           Double -> Double -> Double -> Double -> Double -> Double -> m ()
    translate :: Double -> Double -> m ()
    scale :: Double -> Double -> m ()
    newImage :: Int -> Int -> m (Image m)
    builtinImage :: Text -> m (Maybe (Image m))
    withImage :: Image m -> m a -> m a
    drawImage :: Image m -> Int -> Int -> Int -> Int -> m ()
    imgToCanvas :: Img -> m (Image m)
    globalCompositeOperation :: Text -> m ()
    lineWidth :: Double -> m ()
    strokeColor :: Int -> Int -> Int -> Double -> m ()
    fillColor :: Int -> Int -> Int -> Double -> m ()
    font :: Text -> m ()
    textCenter :: m ()
    textStart :: m ()
    textMiddle :: m ()
    textBottom :: m ()
    beginPath :: m ()
    closePath :: m ()
    moveTo :: (Double, Double) -> m ()
    lineTo :: (Double, Double) -> m ()
    quadraticCurveTo :: (Double, Double) -> (Double, Double) -> m ()
    bezierCurveTo ::
           (Double, Double) -> (Double, Double) -> (Double, Double) -> m ()
    arc :: Double -> Double -> Double -> Double -> Double -> Bool -> m ()
    rect :: Double -> Double -> Double -> Double -> m ()
    fill :: m ()
    stroke :: m ()
    fillRect :: Double -> Double -> Double -> Double -> m ()
    fillText :: Text -> (Double, Double) -> m ()
    measureText :: Text -> m Double
    isPointInPath :: (Double, Double) -> m Bool
    isPointInStroke :: (Double, Double) -> m Bool
    getScreenWidth :: m Double
    getScreenHeight :: m Double

saveRestore :: MonadCanvas m => m a -> m a
saveRestore m = do
    save
    r <- m
    restore
    return r

#if defined(ghcjs_HOST_OS)

data CanvasM a = CanvasM
    { unCanvasM :: (Double, Double) -> Canvas.Context -> IO a
    } deriving (Functor)

runCanvasM :: (Double, Double) -> Canvas.Context -> CanvasM a -> IO a
runCanvasM dim ctx m = unCanvasM m dim ctx

instance Applicative CanvasM where
    pure x = CanvasM (\_ _ -> return x)
    f <*> x = CanvasM (\dim ctx -> unCanvasM f dim ctx <*> unCanvasM x dim ctx)

instance Monad CanvasM where
    return = pure
    m >>= f = CanvasM $ \dim ctx -> do
        x <- unCanvasM m dim ctx
        unCanvasM (f x) dim ctx

foreign import javascript "$2.globalCompositeOperation = $1;"
               js_globalCompositeOperation :: JSString -> Canvas.Context -> IO ()

foreign import javascript "$r = $3.isPointInPath($1, $2);"
               js_isPointInPath :: Double -> Double -> Canvas.Context -> IO Bool

foreign import javascript "$r = $3.isPointInStroke($1, $2);"
               js_isPointInStroke :: Double -> Double -> Canvas.Context -> IO Bool

instance MonadIO CanvasM where
    liftIO action = CanvasM $ \_ _ -> action

instance MonadCanvas CanvasM where
    type Image CanvasM = Canvas.Canvas
    save = CanvasM (const Canvas.save)
    restore = CanvasM (const Canvas.restore)
    transform a b c d e f = CanvasM (const (Canvas.transform a b c d e f))
    translate x y = CanvasM (const (Canvas.translate x y))
    scale x y = CanvasM (const (Canvas.scale x y))
    newImage w h = liftIO (Canvas.create w h)
    builtinImage name = liftIO $ do
        Just doc <- currentDocument
        canvas <- getElementById doc (textToJSString name)
        return (Canvas.Canvas . unElement <$> canvas)
    withImage img m = liftIO $ do
        ctx <- Canvas.getContext img
        w <- realToFrac <$> Canvas.width img
        h <- realToFrac <$> Canvas.height img
        unCanvasM m (w, h) ctx
    drawImage (Canvas.Canvas c) x y w h =
        CanvasM (const (Canvas.drawImage (Canvas.Image c) x y w h))
    imgToCanvas = liftIO . imgToCanvas'
    globalCompositeOperation op =
        CanvasM (const (js_globalCompositeOperation (textToJSString op)))
    lineWidth w = CanvasM (const (Canvas.lineWidth w))
    strokeColor r g b a = CanvasM (const (Canvas.strokeStyle r g b a))
    fillColor r g b a = CanvasM (const (Canvas.fillStyle r g b a))
    font t = CanvasM (const (Canvas.font (textToJSString t)))
    textCenter = CanvasM (const (Canvas.textAlign Canvas.Center))
    textStart = CanvasM (const (Canvas.textAlign Canvas.Start))
    textMiddle = CanvasM (const (Canvas.textBaseline Canvas.Middle))
    textBottom = CanvasM (const (Canvas.textBaseline Canvas.Bottom))
    beginPath = CanvasM (const Canvas.beginPath)
    closePath = CanvasM (const Canvas.closePath)
    moveTo (x, y) = CanvasM (const (Canvas.moveTo x y))
    lineTo (x, y) = CanvasM (const (Canvas.lineTo x y))
    quadraticCurveTo (x1, y1) (x2, y2) =
        CanvasM (const (Canvas.quadraticCurveTo x1 y1 x2 y2))
    bezierCurveTo (x1, y1) (x2, y2) (x3, y3) =
        CanvasM (const (Canvas.bezierCurveTo x1 y1 x2 y2 x3 y3))
    arc x y r a1 a2 dir = CanvasM (const (Canvas.arc x y r a1 a2 dir))
    rect x y w h = CanvasM (const (Canvas.rect x y w h))
    fill = CanvasM (const Canvas.fill)
    stroke = CanvasM (const Canvas.stroke)
    fillRect x y w h = CanvasM (const (Canvas.fillRect x y w h))
    fillText t (x, y) = CanvasM (const (Canvas.fillText (textToJSString t) x y))
    measureText t = CanvasM (const (Canvas.measureText (textToJSString t)))
    isPointInPath (x, y) = CanvasM (const (js_isPointInPath x y))
    isPointInStroke (x, y) = CanvasM (const (js_isPointInStroke x y))
    getScreenWidth = CanvasM $ \(w, _) _ -> return w
    getScreenHeight = CanvasM $ \(_, h) _ -> return h

imgToCanvas' :: Img -> IO Canvas.Canvas
imgToCanvas' (StringImg imgid) = do
    mbdoc <- currentDocument
    case mbdoc of
        Nothing -> error $ "imgToCanvas doc"
        Just doc -> do
            mbel <- getElementById doc (fromString imgid :: JSString)
            case mbel of
                Nothing -> error $ "imgToCanvas element " ++ show imgid
                Just el -> do
                    val <- toJSVal el
                    return $ Canvas.Canvas val
imgToCanvas' (HTMLImg e) = do
    val <- toJSVal e
    return $ Canvas.Canvas val
imgToCanvas' (CanvasImg c) = return c

--runCanvasIO :: (Int,Int) -> CanvasM a -> IO a
--runCanvasIO (w,h) m = do
--    ctx <- getCWContext
--    runCanvasM (realToFrac w,realToFrac h) ctx m

img2Canvas :: Int -> Int -> Img -> IO Canvas.Canvas
img2Canvas w h (CanvasImg c) = return c
img2Canvas w h img = do
    cimg <- imgToCanvas' img
    canvas <- Canvas.create w h
    ctx <- Canvas.getContext canvas
    runCanvasM (realToFrac w,realToFrac h) ctx $ drawImage cimg 0 0 w h
    return canvas

getImageData :: Canvas.Context -> Int -> Int -> Int -> Int -> IO ImageData
getImageData c l t w h = js_getImageData c l t w h
{-# INLINE getImageData #-}

foreign import javascript unsafe "$1.getImageData($2,$3,$4,$5)"
  js_getImageData :: Canvas.Context -> Int -> Int -> Int -> Int -> IO ImageData
  
putImageData :: Canvas.Context -> ImageData -> Int -> Int -> IO ImageData
putImageData c idta w h = js_putImageData c idta w h
{-# INLINE putImageData #-}

foreign import javascript unsafe "$1.putImageData($2,$3,$4)"
  js_putImageData :: Canvas.Context -> ImageData -> Int -> Int -> IO ImageData

createImageData :: Canvas.Context -> Int -> Int -> IO ImageData
createImageData c w h = js_createImageData c w h
{-# INLINE createImageData #-}

foreign import javascript unsafe "$1.createImageData($2,$3)"
  js_createImageData :: Canvas.Context -> Int -> Int -> IO ImageData

toJuicyImage :: Image CanvasM -> IO (Juicy.Image Juicy.PixelRGBA8)
toJuicyImage img = do
    ctx <- Canvas.getContext img
    w <- Canvas.width img
    h <- Canvas.height img
    imgdata <- getImageData ctx 0 0 w h
    let dta = ImageData.getData imgdata
    let sz = w*h*4
    let go i = if i == sz
            then return Nothing
            else do
                y <- unsafeIndex i dta
                return $ Just (y,i+1)
    vdta <- V.unfoldrM go 0
    return $ Juicy.Image w h vdta

fromJuicyImage :: Juicy.Image Juicy.PixelRGBA8 -> IO (Image CanvasM)
fromJuicyImage (Juicy.Image w h dta) = do
    c <- Canvas.create w h
    ctx <- Canvas.getContext c
    cidta <- createImageData ctx w h
    let cdta = ImageData.getData cidta
    V.imapM_ (\i px -> unsafeSetIndex i px cdta) dta
    putImageData ctx cidta 0 0
    return c

instance TypedArray Uint8ClampedArray where
  index i a                   = IO (indexW8 i a)
  unsafeIndex i a             = IO (unsafeIndexW8 i a)
  setIndex i (W8# x) a        = IO (js_setIndexW i x a)
  unsafeSetIndex i (W8# x) a  = IO (js_unsafeSetIndexW i x a)
  indexOf s (W8# x) a         = IO (js_indexOfW s x a)
  lastIndexOf s (W8# x) a     = IO (js_lastIndexOfW s x a)
  create l                    = IO (js_createUint8ClampedArray l)
  fromArray a                 = uint8ClampedArrayFrom a
  fromArrayBuffer b           = undefined

#else

-- Unfortunately, the Canvas monad from blank-canvas lacks a MonadIO instance.
-- We can recover it by inserting send calls where needed.  This looks a lot
-- like a free monad, but we want our own interpreter logic, so it's written
-- by hand.

data CanvasM a = CanvasOp (Maybe Canvas.CanvasContext) (Canvas (CanvasM a))
               | NativeOp (Canvas.DeviceContext -> IO (CanvasM a))
               | PureOp a
    deriving (Functor)

doCanvas :: Maybe Canvas.CanvasContext -> Canvas a -> Canvas a
doCanvas Nothing m = m
doCanvas (Just ctx) m = Canvas.with ctx m

interpCanvas :: CanvasM a -> Canvas (CanvasM a)
interpCanvas (CanvasOp mctx op) = doCanvas mctx op >>= interpCanvas
interpCanvas other = return other

runCanvasM :: Canvas.DeviceContext -> CanvasM a -> IO a
runCanvasM _    (PureOp   a)   = return a
runCanvasM dctx (NativeOp fm) = fm dctx >>= runCanvasM dctx
runCanvasM dctx m             = Canvas.send dctx (interpCanvas m) >>= runCanvasM dctx

instance Applicative CanvasM where
    pure = PureOp

    (CanvasOp mctx1 f) <*> (CanvasOp mctx2 x) = CanvasOp mctx1 (fmap (<*>) f <*> doCanvas mctx2 x)
    f <*> x = f `ap` x

instance Monad CanvasM where
    return = pure
    PureOp x >>= f = f x
    NativeOp op >>= f = NativeOp $ \dctx -> do
        next <- op dctx
        return (next >>= f)
    CanvasOp mctx op >>= f = CanvasOp mctx $ bindCanvas (doCanvas mctx op) f

bindCanvas :: Canvas (CanvasM a) -> (a -> CanvasM b) -> Canvas (CanvasM b)
bindCanvas m cont = do
    next <- m
    case next of
        CanvasOp mctx op -> bindCanvas (doCanvas mctx op) cont
        _                -> return (next >>= cont)

instance MonadIO CanvasM where
    liftIO x = NativeOp $ const $ PureOp <$> x

liftCanvas :: Canvas a -> CanvasM a
liftCanvas m = CanvasOp Nothing (PureOp <$> m)

instance MonadCanvas CanvasM where
    type Image CanvasM = Canvas.CanvasContext

    save = liftCanvas $ Canvas.save ()
    restore = liftCanvas $ Canvas.restore ()
    transform a b c d e f = liftCanvas $ Canvas.transform (a, b, c, d, e, f)
    translate x y = liftCanvas $ Canvas.translate (x, y)
    scale x y = liftCanvas $ Canvas.scale (x, y)
    newImage w h = liftCanvas $ Canvas.newCanvas (w, h)
    builtinImage name = return Nothing

    withImage ctx (CanvasOp Nothing m) = CanvasOp (Just ctx) m
    withImage _   (CanvasOp mctx m)    = CanvasOp mctx m
    withImage ctx (NativeOp fm)        = NativeOp $ \dctx -> withImage ctx <$> fm dctx
    withImage _   (PureOp x)           = PureOp x

    drawImage img x y w h = liftCanvas $
        Canvas.drawImageSize
            ( img
            , fromIntegral x
            , fromIntegral y
            , fromIntegral w
            , fromIntegral h)
    imgToCanvas _ = error $ "imgToCanvas"
    globalCompositeOperation op = liftCanvas $
        Canvas.globalCompositeOperation op
    lineWidth w = liftCanvas $ Canvas.lineWidth w
    strokeColor r g b a = liftCanvas $ Canvas.strokeStyle
        (pack (printf "rgba(%d,%d,%d,%.2f)" r g b a))
    fillColor r g b a = liftCanvas $ Canvas.fillStyle
        (pack (printf "rgba(%d,%d,%d,%.2f)" r g b a))
    font t = liftCanvas $ Canvas.font t
    textCenter = liftCanvas $ Canvas.textAlign Canvas.CenterAnchor
    textMiddle = liftCanvas $ Canvas.textBaseline Canvas.MiddleBaseline
    beginPath = liftCanvas $ Canvas.beginPath ()
    closePath = liftCanvas $ Canvas.closePath ()
    moveTo (x, y) = liftCanvas $ Canvas.moveTo (x, y)
    lineTo (x, y) = liftCanvas $ Canvas.lineTo (x, y)
    quadraticCurveTo (x1, y1) (x2, y2) = liftCanvas $
        Canvas.quadraticCurveTo (x1, y1, x2, y2)
    bezierCurveTo (x1, y1) (x2, y2) (x3, y3) = liftCanvas $
        Canvas.bezierCurveTo (x1, y1, x2, y2, x3, y3)
    arc x y r a1 a2 dir = liftCanvas $
        Canvas.arc (x, y, r, a1, a2, dir)
    rect x y w h = liftCanvas $ Canvas.rect (x, y, w, h)
    fill = liftCanvas $ Canvas.fill ()
    stroke = liftCanvas $ Canvas.stroke ()
    fillRect x y w h = liftCanvas $ Canvas.fillRect (x, y, w, h)
    fillText t (x, y) = liftCanvas $ Canvas.fillText (t, x, y)
    measureText t = liftCanvas $ do
        Canvas.TextMetrics w <- Canvas.measureText t
        return w
    isPointInPath (x, y) = liftCanvas $ Canvas.isPointInPath (x, y)
    isPointInStroke (x, y) = liftCanvas $ return False
    getScreenWidth = liftCanvas $ Canvas.width <$> Canvas.myCanvasContext
    getScreenHeight = liftCanvas $ Canvas.height <$> Canvas.myCanvasContext

#endif
