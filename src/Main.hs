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

import qualified System.IO as IO
import Text.Printf (hPrintf, )

import Control.Monad (when, )

import qualified Data.Map as Map; import Data.Map (Map, )
import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (foldMap, )
import Data.List.HT (partition, )
import Data.Tuple.HT (mapSnd, )


type Diag = Diagram PS.B

timeStep :: Double
timeStep = 0.1

numLong :: Int
numLong = 15

horsep, versep :: Double
horsep = 40
versep = 60

horlen, verlen :: Int -> Double
horlen n = fromIntegral n * horsep
verlen n = fromIntegral n * versep

normalLW, thickLW :: Diag -> Diag
normalLW = lwO 5
thickLW = lwO 8


labels :: Diag
labels =
   hcat' (with & sep .~ horsep) $
   map (\str -> text str # fontSizeL horsep) $
   map (:[]) "CDEFGABCDEFGABC"

long :: Int -> Diag
long numIntervals =
   hcat' (with & sep .~ horsep) $
   map (\(wid, col) ->
          vrule (verlen numIntervals) # wid # lc col) $
      let l = (normalLW, black); h = (thickLW, darkgreen)
      in  [l, l, h, l, h, l, h, l, h, l, h, l, l, l, l]

across :: Int -> Diag
across numIntervals =
   vcat' (with & sep .~ versep) $
   take (succ numIntervals) $ cycle $
      let len = horlen (numLong-1)
      in  map (# normalLW) [hrule len, hrule len # lc grey]

grid :: Int -> Diag
grid numIntervals =
   alignTL (long numIntervals) <>
   alignTL (across numIntervals) <>
   translateY (0.5*versep) labels <>
   (translateX (-0.5*horsep) $ alignBL $ lc white $
    rect (horlen numLong) (1.5*horsep))


dots :: [((Int, Double), Bool)] -> Diag
dots poss =
   flip foldMap poss $
   \((x,y),whole) ->
      translate (r2 (horlen x, - versep * y)) $
      if whole
        then normalLW $ fc blue $ circle (horsep*0.25)
        else (fontSizeL horsep $ text "#") <>
             (lwO 0 $ fc yellow $ circle (horsep*0.4))

noteMap :: Map Int (Int, Bool)
noteMap =
   let o = True; x = False
       semitones = cycle [o,x,o,x,o,o,x,o,x,o,x,o]
   in  Map.fromList $ take 25 $ zip [0..] $
       zip
          (NonEmpty.tail $ NonEmpty.scanl (+) (-1) $ map fromEnum semitones)
          semitones

layoutDots :: MidiFile.T -> [((Int, Double), Bool)]
layoutDots (MidiFile.Cons typ division tracks) =
   map (\(t,(p,whole)) -> ((p,t), whole)) $
   AbsEventList.toPairList $
   AbsEventList.mapTime ((/timeStep) . realToFrac) $
   AbsEventList.mapMaybe
      (\fev -> do
         FileEvent.MIDIEvent ev <- Just fev
         (_c, (_v, p, True)) <- Query.noteExplicitOff ev
         flip Map.lookup noteMap $
            VoiceMsg.subtractPitch (VoiceMsg.toPitch 60) p) $
   EventList.toAbsoluteEventList 0 $
   MidiFile.mergeTracks typ $
   map (MidiFile.secondsFromTicks division) tracks

diag :: FilePath -> IO Diag
diag path = do
   midi <- Load.fromFile path
   let cloud = layoutDots midi
       (tooSmall, (fitting, tooBig)) =
         mapSnd (partition ((<=numLong) . fst . fst)) $
         partition ((<0) . fst . fst) cloud
   when (not $ null tooSmall) $
      hPrintf IO.stderr "Warning: %i notes are too low\n" $ length tooSmall
   when (not $ null tooBig) $
      hPrintf IO.stderr "Warning: %i notes are too high\n" $ length tooBig
   return $ dots fitting <> (grid $ ceiling $ maximum $ map (snd . fst) cloud)

main :: IO ()
main = PS.mainWith diag
