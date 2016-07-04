{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main)
where

import Control.Concurrent
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
-- import           Data.GraphViz.Attributes.Complete
-- import           Data.GraphViz.Commands
import Data.Text (Text)
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
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

import Graph
import GraphStream

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

{-
Note that renaming is a universal problem everywhere.
Lambda calculus has it (variable names),
graphs have it (node/edge IDs), etc.
It would be great if IDs were only informative but the relation itself
would be encoded differently.
Would ABTs help us here?
-}

realGraph :: Graph -> Gr Text Text
realGraph Graph{..} = mkGraph nodes' edges'
  where
    nodes' = map (\n -> nodeId n) nodes
    edges' = map mkEdge edges
    mkEdge Edge{..} = (fst edgeId, snd edgeId, edgeLabel)

renderGraph :: Graph -> IO (Cairo.Render ())
renderGraph graph = do
    let params :: GraphvizParams Int v e () v
        params = defaultDiaParams
                 { fmtEdge = const [arrowTo noArrow] }
    let real = realGraph graph
    print real
    graph' <- layoutGraph' params Dot real
    let drawing :: Diagram B
        drawing = drawGraph
                       (const $ place (circle 19))
                       (\_ _ _ _ _ p -> stroke p)
                       graph'
    pure . snd . rd $ drawing # frame 1

main :: IO ()
main = do
    initializeAll
    window <- createWindow "Dynamic Graph Viewer" defaultWindow
    renderer <- createRenderer window (-1) defaultRenderer

    loop renderer empty

textureSize :: V2 CInt
textureSize = CInt . fromIntegral <$> size'
  where
    size' :: V2 Int
    size' = V2 790 590

loop :: Renderer -> Graph -> IO ()
loop renderer graph = do
    graphOp <- generate arbitrary
    let graph' = applyGraphOp graph graphOp
    cairoRender <- renderGraph graph'
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
    loop renderer graph'
