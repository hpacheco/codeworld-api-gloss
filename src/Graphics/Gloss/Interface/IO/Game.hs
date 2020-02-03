{-# LANGUAGE TupleSections, ExplicitForAll #-}

-- | This game mode lets you manage your own input. Pressing ESC will not abort the program.
--   You also don't get automatic pan and zoom controls like with `display`.
module Graphics.Gloss.Interface.IO.Game
        ( module Graphics.Gloss.Data.Display
        , module Graphics.Gloss.Data.Picture
        , module Graphics.Gloss.Data.Color
        , playIO,playFitScreenIO
        , Event(..), Key(..), SpecialKey(..), MouseButton(..), KeyState(..))
where
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Event
import qualified CodeWorld as CW
import Control.Monad
import qualified Data.Text as Text

-- | Play a game in a window, using IO actions to build the pictures. 
playIO  :: forall world
        .  Display                      -- ^ Display mode.
        -> Color                        -- ^ Background color.
        -> Int                          -- ^ Framerate
        -> world                        -- ^ The initial world.
        -> (world -> IO Picture)        -- ^ An action to convert the world a picture.
        -> (Event -> world -> IO world) -- ^ A function to handle input events.
        -> (Float -> world -> IO world) -- ^ A function to step the world one iteration.
                                        --   It is passed the period of time (in seconds) needing to be advanced.
        -> IO ()

playIO display back framerate start draw react step = CW.ioInteractionOf (display,start) stepCW reactCW (colorToCW back) drawCW
    where
    stepCW f (disp,w) = liftM (disp,) $ step (realToFrac f) w
    reactCW e (disp,w) = do
        --case e of { CW.TimePassing {} -> return (); otherwise -> CW.traceIO (Text.pack $ show e) }
        case eventFromCW disp e of
            Nothing -> return (disp,w)
            Just e@(EventResize (x,y)) -> liftM (Display x y,) $ react e w
            Just e -> do
                --CW.traceIO (Text.pack $ show e)
                liftM (disp,) $ react e w
    drawCW (disp,w) = liftM (displayCWPicture disp) (draw w)

playFitScreenIO :: Display -> Display -> Color -> Int -> world -> (world -> IO Picture) -> (Event -> world -> IO world) -> (Float -> world -> IO world) -> IO ()
playFitScreenIO screen display back framerate start draw react step = CW.ioInteractionOf (screen,start) stepCW reactCW (colorToCW back) drawCW
    where
    stepCW f (disp,w) = liftM (disp,) $ step (realToFrac f) w
    reactCW e (disp,w) = case (eventFromCW disp e >>= fitScreenEvent disp display) of
        Nothing -> return (disp,w)
        Just e@(EventResize (x,y)) -> liftM (Display x y,) $ react e w
        Just e -> liftM (disp,) $ react e w
    drawCW (disp,w) = liftM (displayCWPicture disp . fitScreenPicture disp display) (draw w)


