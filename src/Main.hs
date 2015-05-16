{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import qualified Diagrams.Backend.Postscript.CmdLine as PS
import Diagrams.Prelude


numLong, numAcross :: Int
numLong = 15
numAcross = 30

horsep, versep :: Double
horsep = 0.2
versep = 0.3


labels :: Diagram PS.B
labels =
   hcat' (with & sep .~ horsep) $
   map (\str -> text str # fontSizeL 0.2) $
   map (:[]) "CDEFGABCDEFGABC"

long :: Diagram PS.B
long =
   hcat' (with & sep .~ horsep) $
   map (\(wid, col) ->
          vrule ((fromIntegral numAcross-1) * versep) # lw wid # lc col) $
      let l = (thin, black); h = (thick, darkgreen)
      in  [l, l, h, l, h, l, h, l, h, l, h, l, l, l, l]

across :: Diagram PS.B
across =
   vcat' (with & sep .~ versep) $
   take numAcross $ cycle $
      let len = (fromIntegral numLong-1) * horsep
      in  [hrule len, hrule len # lc grey]

dots :: Diagram PS.B
dots =
   foldr1 atop $
   map
      (\(x,y) -> translate (r2 (x*horsep, -y*versep)) $ fc blue $ circle 0.05)
      [(0,0), (1,1), (3,2), (3,4)]

diag :: Diagram PS.B
diag =
   dots `atop`
   alignTL long `atop` alignTL across `atop`
   translateY 0.15 labels `atop`
   (translateX (-0.1) $ alignBL $ lc white $
    rect (fromIntegral numLong * horsep) (1.5*horsep))

main :: IO ()
main = PS.mainWith diag
