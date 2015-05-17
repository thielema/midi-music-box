{-# LANGUAGE NoMonomorphismRestriction #-}
module Main where

import qualified Sound.MIDI.Message.Class.Query as Query

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File as MidiFile

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Diagrams.Backend.Postscript.CmdLine as PS
import Diagrams.Prelude

import Data.Tuple.HT (swap, )


numLong, numAcross :: Int
numLong = 15
numAcross = 30

horsep, versep :: Double
horsep = 0.2
versep = 0.3

horlen, verlen :: Int -> Double
horlen n = fromIntegral n * horsep
verlen n = fromIntegral n * versep


labels :: Diagram PS.B
labels =
   hcat' (with & sep .~ horsep) $
   map (\str -> text str # fontSizeL 0.2) $
   map (:[]) "CDEFGABCDEFGABC"

long :: Diagram PS.B
long =
   hcat' (with & sep .~ horsep) $
   map (\(wid, col) ->
          vrule (verlen (numAcross-1)) # lw wid # lc col) $
      let l = (thin, black); h = (thick, darkgreen)
      in  [l, l, h, l, h, l, h, l, h, l, h, l, l, l, l]

across :: Diagram PS.B
across =
   vcat' (with & sep .~ versep) $
   take numAcross $ cycle $
      let len = horlen (numLong-1)
      in  [hrule len, hrule len # lc grey]

grid :: Diagram PS.B
grid =
   alignTL long `atop` alignTL across `atop`
   translateY 0.15 labels `atop`
   (translateX (-0.1) $ alignBL $ lc white $
    rect (horlen numLong) (1.5*horsep))


dots :: [(Int, Double)] -> Diagram PS.B
dots poss =
   foldr1 atop $
   flip map poss $
   \(x,y) ->
      translate (r2 (horlen x, - versep * y)) $
      lw veryThin $ fc blue $ circle (horsep/4)

layoutDots :: MidiFile.T -> [(Int, Double)]
layoutDots (MidiFile.Cons typ division tracks) =
   map swap $ AbsEventList.toPairList $
   AbsEventList.mapTime ((/0.1) . realToFrac) $
   AbsEventList.mapMaybe
      (\fev -> do
         FileEvent.MIDIEvent ev <- Just fev
         (_c, (_v, p, True)) <- Query.noteExplicitOff ev
         Just $ VoiceMsg.subtractPitch (VoiceMsg.toPitch 60) p) $
   EventList.toAbsoluteEventList 0 $
   MidiFile.mergeTracks typ $
   map (MidiFile.secondsFromTicks division) tracks

diag :: FilePath -> IO (Diagram PS.B)
diag path = do
   midi <- Load.fromFile path
   return $ dots (layoutDots midi) `atop` grid

main :: IO ()
main = PS.mainWith diag
