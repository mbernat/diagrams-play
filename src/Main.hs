{-# LANGUAGE NoMonomorphismRestriction #-}

-- import           Diagrams.Backend.Rasterific.CmdLine
import           Diagrams.Backend.Gtk
import           Diagrams.Backend.Cairo
import           Diagrams.Prelude
import           Diagrams.TwoD.GraphViz

import           Data.GraphViz
import           Data.GraphViz.Attributes.Complete
import           Data.GraphViz.Commands

import Graphics.UI.Gtk.Misc.DrawingArea
import Graphics.UI.Gtk.Gdk.DrawWindow
import Graphics.UI.Gtk.General.General
import Graphics.UI.Gtk.Windows.Window
import Graphics.UI.Gtk.Abstract.Widget
import Graphics.UI.Gtk.Abstract.Container

hex = mkGraph [0..19]
        (   [ (v, (v+1)`mod`6, ()) | v <- [0..5] ]
         ++ [ (v, v+k, ()) | v <- [0..5], k <- [6,12] ]
         ++ [ (2,18,()), (2,19,()), (15,18,()), (15,19,()), (18,3,()), (19,3,()) ]
        )

main = do
  let params :: GraphvizParams Int v e () v
      params = defaultDiaParams
               { fmtEdge = const [arrowTo noArrow] }
  hex' <- layoutGraph' params Dot hex
  let hexDrawing :: Diagram B
      hexDrawing = drawGraph
                     (const $ place (circle 19))
                     (\_ _ _ _ _ p -> stroke p)
                     hex'
--  mainWith $ hexDrawing # frame 1

  initGUI
  window <- windowNew
  drawingArea <- drawingAreaNew
  containerAdd window drawingArea
  widgetShowAll window
  drawingArea `onExpose` \_event -> do
      drawWindow <- widgetGetDrawWindow drawingArea
      renderToGtk drawWindow $ hexDrawing # frame 1
      return True

  mainGUI

