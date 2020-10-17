module Estuary.Languages.CineCer0.Parser (cineCer0) where

import Language.Haskell.Exts
import Control.Applicative
import Data.IntMap.Strict
import Data.Time
import Language.Haskellish
import Data.Bifunctor
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes)

import Estuary.Languages.CineCer0.VideoSpec
import Estuary.Languages.CineCer0.Spec
import Estuary.Languages.CineCer0.Signal

type H = Haskellish ()

cineCer0 :: UTCTime -> String -> Either String Spec
cineCer0 eTime x = do
  let sourceAsList = "[" ++ (intercalate "," $ fmap (++ " _0") $ splitOn ";" x) ++ "\n]"
  sourceAsExp <- case parseExp sourceAsList of
    ParseFailed l e -> Left e
    ParseOk e -> Right e
  (theSpec,_) <- runHaskellish (spec eTime) () sourceAsExp
  return theSpec

-- borrowing a trick from Punctual to resolve an issue with comments and newlines...
-- the "redundant" argument _0 applied to something just yields that same thing
_0Arg :: H a -> H a
_0Arg p = p <|> fmap fst (functionApplication p $ reserved "_0")

spec :: UTCTime -> H Spec
spec eTime = do
  os <- fmap (fromList . zip [0..] . catMaybes) $ list maybeLayerSpec
  return $ Spec {
    evalTime = eTime,
    layerSpecMap = os
  }

maybeLayerSpec :: H (Maybe LayerSpec)
maybeLayerSpec = _0Arg $
  (Just <$> layerSpec) <|>
  Nothing <$ reserved "_0"

layerSpec :: H LayerSpec
layerSpec = _0Arg $
  (vs_vs <*> layerSpec) <|>
  (layerSpecFunc <*> string) <|>
  (fmap stringToLayerSpec string)

layerSpecFunc :: H (String -> LayerSpec)
layerSpecFunc =
  textToLayerSpec <$ reserved "text" <|>
  videoToLayerSpec <$ reserved "video"


-- //////////////

int :: H Int
int = fromIntegral <$> integer

ndt :: H NominalDiffTime
ndt = fromRational <$> rationalOrInteger

-- //////////////

sigMayRat :: H (Signal (Maybe Rational))
sigMayRat =
  rat_sigMayRat <*> rationalOrInteger <|>
  (constantSignal . Just) <$> rationalOrInteger

rat_sigMayRat :: H (Rational -> Signal (Maybe Rational))
rat_sigMayRat = rat_rat_sigMayRat <*> rationalOrInteger

rat_rat_sigMayRat :: H (Rational -> Rational -> Signal (Maybe Rational))
rat_rat_sigMayRat = ndt_rat_rat_sigMayRat <*> ndt

ndt_rat_rat_sigMayRat :: H (NominalDiffTime -> Rational -> Rational -> Signal (Maybe Rational))
ndt_rat_rat_sigMayRat = ramp2 <$ reserved "ramp"

sigInt :: H (Signal Int)
sigInt = constantSignal <$> int

sigStr :: H (Signal String)
sigStr = constantSignal <$> string

sigRat :: H (Signal Rational)
sigRat =
  rat_sigRat <*> rationalOrInteger <|>
  constantSignal <$> rationalOrInteger

rat_sigRat :: H (Rational -> Signal Rational)
rat_sigRat = rat_rat_sigRat <*> rationalOrInteger

rat_rat_sigRat :: H (Rational -> Rational -> Signal Rational)
rat_rat_sigRat = ndt_rat_rat_sigRat <*> ndt

ndt_rat_rat_sigRat :: H (NominalDiffTime -> Rational -> Rational -> Signal Rational)
ndt_rat_rat_sigRat = ramp <$ reserved "ramp"

-- new fade in (syntax sugar funcs)
ndt_sigRat :: H (NominalDiffTime -> Signal Rational)
ndt_sigRat = 
  (reserved "fadeIn" >> return fadeIn) <|>
  (reserved "fadeOut" >> return fadeOut)


-- //////////////

vs_vs :: H (LayerSpec -> LayerSpec)
vs_vs =
  sigRat_vs_vs <*> sigRat <|>
  sigMayRat_vs_vs <*> sigMayRat <|>
  rat_vs_vs <*> rationalOrInteger <|>
  sigStr_vs_vs <*> sigStr <|>
  sigInt_vs_vs <*> sigInt <|>
  (reserved "strike" >> return setStrike) <|>
  (reserved "bold" >> return setBold) <|>
  (reserved "italic" >> return setItalic)
  -- <|>
 -- (reserved "mute" >> return setMute) <|>
 -- (reserved "unmute" >> return setUnmute)

sigMayRat_vs_vs :: H (Signal (Maybe Rational) -> LayerSpec -> LayerSpec)
sigMayRat_vs_vs =
  setOpacity <$ reserved "setOpacity" <|>
  shiftOpacity <$ reserved "opacity" <|>
  setBlur <$ reserved "setBlur" <|>
  shiftBlur <$ reserved "blur" <|>
  setBrightness <$ reserved "setBrightness" <|>
  shiftBrightness <$ reserved "brightness" <|>
  setContrast <$ reserved "setContrast" <|>
  shiftContrast <$ reserved "contrast" <|>
  setGrayscale <$ reserved "setGrayscale" <|>
  shiftGrayscale <$ reserved "grayscale" <|>
  setSaturate <$ reserved "setSaturate" <|>
  shiftSaturate <$ reserved "saturate"

sigRat_vs_vs :: H (Signal Rational -> LayerSpec -> LayerSpec)
sigRat_vs_vs =
  setPosX <$ reserved "setPosX" <|>
  shiftPosX <$ reserved "posX" <|>
  setPosY <$ reserved "setPosY" <|>
  shiftPosY <$ reserved "posY" <|>
  setWidth <$ reserved "setWidth" <|>
  shiftWidth <$ reserved "width" <|>
  setHeight <$ reserved "setHeight" <|>
  shiftHeight <$ reserved "height" <|>
  setSize <$ reserved "setSize" <|>
  shiftSize <$ reserved "size" <|>
  circleMask <$ reserved "circleMask" <|>
  sqrMask <$ reserved "sqrMask" <|>
  setVolume <$ reserved "vol" <|>
  setFontSize <$ reserved "fontSize" <|>
  sigRat_sigRat_vs_vs <*> sigRat

sigInt_vs_vs :: H (Signal Int -> LayerSpec -> LayerSpec)
sigInt_vs_vs =
  setZIndex <$ reserved "z"

sigStr_vs_vs :: H (Signal String -> LayerSpec -> LayerSpec)
sigStr_vs_vs =
  setColour <$ reserved "colour" -- this is wrong, the type is actually Colour not String
  setFontFamily <$ reserved "font"

sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> LayerSpec -> LayerSpec)
sigRat_sigRat_vs_vs =
  setCoord <$ reserved "setCoord" <|>
  shiftCoord <$ reserved "coord" <|>
  sigRat_sigRat_sigRat_vs_vs <*> sigRat

sigRat_sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> Signal Rational -> LayerSpec -> LayerSpec)
sigRat_sigRat_sigRat_vs_vs =
  circleMask' <$ reserved "circleMask'" <|>
  setRGB <$ reserved "colour" <|>
  setHSV <$ reserved "colour'" <|>
  sigRat_sigRat_sigRat_sigRat_vs_vs <*> sigRat

sigRat_sigRat_sigRat_sigRat_vs_vs :: H (Signal Rational -> Signal Rational -> Signal Rational -> Signal Rational -> LayerSpec -> LayerSpec)
sigRat_sigRat_sigRat_sigRat_vs_vs =
  rectMask <$ reserved "rectMask"

-- ////

rat_vs_vs :: H (Rational -> LayerSpec -> LayerSpec)
rat_vs_vs =
  playNatural <$ reserved "natural" <|>
  playSnap <$ reserved "snap" <|>
  playSnapMetre <$ reserved "snapMetre" <|>
  rat_rat_vs_vs <*> rationalOrInteger <|>
  ndt_rat_vs_vs <*> ndt

rat_rat_vs_vs :: H (Rational -> Rational -> LayerSpec -> LayerSpec)
rat_rat_vs_vs =
  playEvery <$ reserved "every" <|>
  rat_rat_rat_vs_vs <*> rationalOrInteger <|>
  ndt_rat_rat_vs_vs <*> ndt <|>
  quant <$ reserved "quant"

rat_rat_rat_vs_vs :: H (Rational -> Rational -> Rational -> LayerSpec -> LayerSpec)
rat_rat_rat_vs_vs =
  playChop' <$ reserved "chop'" <|>
  rat_rat_rat_rat_vs_vs <*> rationalOrInteger

rat_rat_rat_rat_vs_vs :: H (Rational -> Rational -> Rational -> Rational -> LayerSpec -> LayerSpec)
rat_rat_rat_rat_vs_vs =
  playChop <$ reserved "chop"

ndt_rat_vs_vs :: H (NominalDiffTime -> Rational -> LayerSpec -> LayerSpec)
ndt_rat_vs_vs =
  playNow <$ reserved "now"

ndt_rat_rat_vs_vs :: H (NominalDiffTime -> Rational -> Rational -> LayerSpec -> LayerSpec)
ndt_rat_rat_vs_vs =
  ndt_ndt_rat_rat_vs_vs <*> ndt

ndt_ndt_rat_rat_vs_vs :: H (NominalDiffTime -> NominalDiffTime -> Rational -> Rational -> LayerSpec -> LayerSpec)
ndt_ndt_rat_rat_vs_vs = playChopSecs <$ reserved "chopSecs" -- time function
