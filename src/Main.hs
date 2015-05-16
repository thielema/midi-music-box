{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import Diagrams.Prelude
import Diagrams.Backend.Postscript.CmdLine


numLong, numAcross :: Int
numLong = 15
numAcross = 30

horsep, versep :: Double
horsep = 0.2
versep = 0.3


labels :: Diagram B
labels =
   hcat' (with & sep .~ horsep) $
   map (\str -> text str # fontSizeL 0.2) $
   map (:[]) "CDEFGABCDEFGABC"

long :: Diagram B
long =
   hcat' (with & sep .~ horsep) $
   map (\(wid, col) ->
          vrule ((fromIntegral numAcross-1) * versep) # lw wid # lc col) $
      let l = (thin, black); h = (thick, darkgreen)
      in  [l, l, h, l, h, l, h, l, h, l, h, l, l, l, l]

across :: Diagram B
across =
   vcat' (with & sep .~ versep) $
   take numAcross $ cycle $
      let len = (fromIntegral numLong-1) * horsep
      in  [hrule len, hrule len # lc grey]

diag :: Diagram B
diag =
   alignTL long `atop` alignTL across `atop`
   translateY 0.1 labels `atop`
   (translateX (-0.1) $ alignBL $ lc white $
    rect (fromIntegral numLong * horsep) horsep)

main :: IO ()
main = mainWith diag
