-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.
import Data.Maybe
import Expr
import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
       -- The markup "<i>...</i>" means that the text inside should be rendered
       -- in italics.

     -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas


readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     let expression = fromMaybe (Num 0) (readExpr formula)
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line.
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" (points expression 0.04 (canWidth, canHeight)) canvas

-- H --------------------------------------------------------------------------

points :: Expr -> Double -> (Int,Int) -> [Point]
points expression scale (width,height) = zip allX allY
  where
    pixToReal :: Double -> Double
    pixToReal x = scale * (x - fromIntegral width / 2)
    realToPix :: Double -> Double
    realToPix y = y / negate scale + fromIntegral height / 2
    allX = [0..fromIntegral width]
    allRealX = map pixToReal allX
    allY = map (realToPix . eval expression) allRealX 
  
