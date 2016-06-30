{-# LANGUAGE OverloadedStrings #-}
module Main (main)
where

import Control.Concurrent
import Data.GraphViz
-- import           Data.GraphViz.Attributes.Complete
-- import           Data.GraphViz.Commands
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal
import Diagrams.Prelude
import Diagrams.TwoD.GraphViz
import Foreign.C.Types
import qualified Graphics.Rendering.Cairo as Cairo
import Linear (V4(..))
import SDL (($=))
import SDL.Cairo
import SDL.Init
import SDL.Video
import System.Random

--import GraphStream
import DivisionGraph

--q :: GraphStream
--q = []

rd :: Diagram B -> (IO (), Cairo.Render ())
rd = renderDia Cairo opts
  where
    -- This is only necessary when rendering to a file
    -- but I don't know how to get rid of it.
    opts = CairoOptions
        { _cairoFileName = "foo.png"
        , _cairoSizeSpec = dims 1000   -- SizeSpec V2 Double
        , _cairoOutputType = PNG -- OutputType
        , _cairoBypassAdjust = True
        }

renderGraph :: Integer -> IO (Cairo.Render ())
renderGraph count = do
    let params :: GraphvizParams Int v e () v
        params = defaultDiaParams
                 { fmtEdge = const [arrowTo noArrow] }
    divisionGraph' <- layoutGraph' params Dot (divisionGraph count)
    let drawing :: Diagram B
        drawing = drawGraph
                       (const $ place (circle 19))
                       (\_ _ _ _ _ p -> stroke p)
                       divisionGraph'
    pure . snd . rd $ drawing # frame 1

main :: IO ()
main = do
    initializeAll
    window <- createWindow "Dynamic Graph Viewer" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    loop renderer

textureSize :: V2 CInt
textureSize = CInt . fromIntegral <$> size'
  where
    size' :: V2 Int
    size' = V2 790 590

loop :: Renderer -> IO ()
loop renderer = do
    count <- randomIO
    cairoRender <- renderGraph (count `mod` 50 + 30)
    texture <- createCairoTexture renderer textureSize
    withCairoTexture' texture $ \surface ->
        Cairo.renderWith surface $ do
            Cairo.setSourceRGB 255 255 255
            Cairo.rectangle 5 5 780 580
            Cairo.fill
            cairoRender

    rendererDrawColor renderer $= V4 255 255 255 255
    clear renderer
    -- Region in the main window
    -- Rectangle takes (x, y) point position and (x, y) vector extent
    copy renderer texture Nothing (Just $ Rectangle (P $ V2 5 5) textureSize)
    present renderer

    let delay = 10^(6::Int)
    threadDelay delay
    loop renderer
