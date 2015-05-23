{-# LANGUAGE FlexibleContexts #-}
module Main where

import qualified Sound.MIDI.Message.Class.Query as Query

import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg
import qualified Sound.MIDI.File.Event as FileEvent
import qualified Sound.MIDI.File.Load as Load
import qualified Sound.MIDI.File as MidiFile

import qualified Data.EventList.Absolute.TimeBody as AbsEventList
import qualified Data.EventList.Relative.TimeBody as EventList

import qualified Diagrams.Backend.Postscript.CmdLine as PS
import qualified Diagrams.Backend.CmdLine as Cmd
import Diagrams.Prelude

import qualified Options.Applicative as OP

import qualified System.IO as IO
import Text.Printf (hPrintf, )

import Control.Monad (when, )

import qualified Data.Map as Map; import Data.Map (Map, )
import qualified Data.NonEmpty as NonEmpty
import Data.Foldable (foldMap, fold, )
import Data.Tuple.HT (mapSnd, )


type Diag = Diagram PS.B

numLong :: Int
numLong = 15

globalScale :: Double
globalScale = 1/7

horsep, versep :: Double
horsep = 40*globalScale
versep = 80*globalScale

hormargin, horextramargin :: Double
hormargin = 65*globalScale
horextramargin = 100*globalScale

horlen, verlen :: Int -> Double
horlen n = fromIntegral n * horsep
verlen n = fromIntegral n * versep

normalLW, thickLW :: Diag -> Diag
normalLW = lwO (5*globalScale)
thickLW = lwO (8*globalScale)


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
   translateX (-hormargin)
      (alignTL (vrule (verlen numIntervals) # normalLW # lc grey)) <>
   translateX (horlen (numLong-1) + hormargin)
      (alignTL (vrule (verlen numIntervals) # normalLW # lc grey)) <>
   translateY (0.5*versep) labels <>
   (translateX (-horextramargin) $ translateY (verlen 2) $ alignTL $ lc white $
    rect (horlen (numLong-1) + 2*horextramargin) (verlen (numIntervals+4)))


dots :: [(DotType, (Int, Double))] -> Diag
dots poss =
   flip foldMap poss $
   \(typ, (x,y)) ->
      translate (r2 (horlen x, - versep * y)) $
      let warning txt =
            (fontSizeL horsep $ text txt) <>
            (lwO 0 $ fc yellow $ circle (horsep*0.4))
      in  case typ of
            Valid -> normalLW $ fc blue $ circle (horsep*0.25)
            Semitone -> warning "#"
            TooLow -> warning "!"
            TooHigh -> warning "!"

noteMap :: Map Int (Bool, Int)
noteMap =
   let o = True; x = False
       semitones = cycle [o,x,o,x,o,o,x,o,x,o,x,o]
   in  Map.fromList $ take 25 $ zip [0..] $ zip semitones $
       NonEmpty.tail $ NonEmpty.scanl (+) (-1) $ map fromEnum semitones


data DotType = Valid | Semitone | TooLow | TooHigh
   deriving (Eq, Ord)

layoutDots ::
   TimeStep -> VoiceMsg.Pitch -> MidiFile.T -> [(DotType, (Int, Double))]
layoutDots (TimeStep timeStep) zeroKey (MidiFile.Cons typ division tracks) =
   map (\(t, (dottyp,p)) -> (dottyp, (p,t))) $
   AbsEventList.toPairList $
   AbsEventList.mapTime ((/timeStep) . realToFrac) $
   AbsEventList.mapMaybe
      (\fev -> do
         FileEvent.MIDIEvent ev <- Just fev
         (_c, (_v, p, True)) <- Query.noteExplicitOff ev
         let pz = VoiceMsg.subtractPitch zeroKey p
         return $
            case Map.lookup pz noteMap of
               Just (s, n) -> (if s then Valid else Semitone, n)
               Nothing ->
                  let ( pmin, (_, nmin)) = Map.findMin noteMap
                      (_pmax, (_, nmax)) = Map.findMax noteMap
                  in  if pz < pmin
                        then (TooLow,  nmin)
                        else (TooHigh, nmax)) $
   EventList.toAbsoluteEventList 0 $
   MidiFile.mergeTracks typ $
   map (MidiFile.secondsFromTicks division) tracks


newtype ZeroKey = ZeroKey Int

instance Cmd.Parseable ZeroKey where
   parser =
      OP.option (ZeroKey <$> OP.auto)
         (OP.long "zerokey" OP.<>
          OP.metavar "INT" OP.<>
          OP.value (ZeroKey 60) OP.<>
          OP.help "MIDI key for the lowest note line")


newtype TimeStep = TimeStep Double

instance Cmd.Parseable TimeStep where
   parser =
      OP.option (TimeStep <$> OP.auto)
         (OP.long "timestep" OP.<>
          OP.metavar "SECONDS" OP.<>
          OP.value (TimeStep 0.1) OP.<>
          OP.help "time step between lines")


newtype Input = Input FilePath

instance Cmd.Parseable Input where
   parser = OP.argument (Input <$> OP.str) (OP.metavar "INPUT")


diag :: TimeStep -> ZeroKey -> Input -> IO Diag
diag timeStep (ZeroKey zeroKey) (Input path) = do
   midi <- Load.fromFile path
   let cloud = layoutDots timeStep (VoiceMsg.toPitch zeroKey) midi
       sorted = Map.fromListWith (++) $ map (mapSnd (:[])) cloud
       warning typ msg =
         let n = length $ fold $ Map.lookup typ sorted
         in  when (n > 0) $ hPrintf IO.stderr "Warning: %i %s\n" n msg
   warning Semitone "semitones"
   warning TooLow "notes are too low"
   warning TooHigh "notes are too high"
   return $ dots cloud <> (grid $ ceiling $ maximum $ map (snd . snd) cloud)

main :: IO ()
main = PS.mainWith diag
