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


type Diag = Diagram PS.B

timeStep :: Double
timeStep = 0.1

numLong :: Int
numLong = 15

horsep, versep :: Double
horsep = 0.2
versep = 0.3

horlen, verlen :: Int -> Double
horlen n = fromIntegral n * horsep
verlen n = fromIntegral n * versep


labels :: Diag
labels =
   hcat' (with & sep .~ horsep) $
   map (\str -> text str # fontSizeL 0.2) $
   map (:[]) "CDEFGABCDEFGABC"

long :: Int -> Diag
long numIntervals =
   hcat' (with & sep .~ horsep) $
   map (\(wid, col) ->
          vrule (verlen numIntervals) # lw wid # lc col) $
      let l = (thin, black); h = (thick, darkgreen)
      in  [l, l, h, l, h, l, h, l, h, l, h, l, l, l, l]

across :: Int -> Diag
across numIntervals =
   vcat' (with & sep .~ versep) $
   take (succ numIntervals) $ cycle $
      let len = horlen (numLong-1)
      in  [hrule len, hrule len # lc grey]

grid :: Int -> Diag
grid numIntervals =
   alignTL (long numIntervals) `atop`
   alignTL (across numIntervals) `atop`
   translateY 0.15 labels `atop`
   (translateX (-0.1) $ alignBL $ lc white $
    rect (horlen numLong) (1.5*horsep))


dots :: [(Int, Double)] -> Diag
dots poss =
   foldr1 atop $
   flip map poss $
   \(x,y) ->
      translate (r2 (horlen x, - versep * y)) $
      lw veryThin $ fc blue $ circle (horsep/4)

layoutDots :: MidiFile.T -> [(Int, Double)]
layoutDots (MidiFile.Cons typ division tracks) =
   map swap $ AbsEventList.toPairList $
   AbsEventList.mapTime ((/timeStep) . realToFrac) $
   AbsEventList.mapMaybe
      (\fev -> do
         FileEvent.MIDIEvent ev <- Just fev
         (_c, (_v, p, True)) <- Query.noteExplicitOff ev
         Just $ VoiceMsg.subtractPitch (VoiceMsg.toPitch 60) p) $
   EventList.toAbsoluteEventList 0 $
   MidiFile.mergeTracks typ $
   map (MidiFile.secondsFromTicks division) tracks

diag :: FilePath -> IO Diag
diag path = do
   midi <- Load.fromFile path
   let cloud = layoutDots midi
   return $ dots cloud `atop` (grid $ ceiling $ maximum $ map snd cloud)

main :: IO ()
main = PS.mainWith diag
