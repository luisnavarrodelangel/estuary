module Estuary.WebDirt.Stream where

import Sound.Tidal.Context
import Control.Concurrent.MVar
import Control.Monad.Loops (iterateM_)
import Control.Monad (liftM)
import Data.Time (getCurrentTime)
import Data.Time.Clock.POSIX
import Data.Map
import qualified Control.Exception as E
import Data.Time
import qualified Estuary.WebDirt.Foreign as WebDirt
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal.Pure as P

webDirtTicksPerCycle :: Integer
webDirtTicksPerCycle = 8

type WebDirtStream = ParamPattern -> IO ()

webDirtStream :: T.JSVal -> IO WebDirtStream
webDirtStream webDirt = do
  -- webDirt <- WebDirt.webDirt
  -- WebDirt.initializeWebAudio webDirt
  x <- WebDirt.getCurrentTime webDirt
  putStrLn (show x)
  let now = posixSecondsToUTCTime $ realToFrac x
  -- mTempo <- newMVar (Tempo {at=now,beat=0.0,cps=1.0,paused=False,clockLatency=0.2})
  mPattern <- newMVar silence
  forkIO $ clockedTickWebDirt webDirt (webDirtTick webDirt mPattern)
  return $ \p -> do swapMVar mPattern p
                    return ()

beatNowWebDirt :: T.JSVal -> Tempo -> IO Double
beatNowWebDirt webDirt t = do
  x <- WebDirt.getCurrentTime webDirt
  putStrLn (show x)
  let now = posixSecondsToUTCTime $ realToFrac x
  let delta = realToFrac $ diffUTCTime now (at t)
  let beatDelta = cps t * delta
  return $ beat t + beatDelta

readTempo webDirt = do
readTempo :: T.JSVal -> IO Tempo
  (time,beats,bpm) <- WebDirt.tempo webDirt
  return $ Tempo {at=time,beat=beats,cps=bpm/60.0,paused=False,clockLatency=0.2}

getCurrentWebDirtBeat :: T.JSVal -> IO Rational
getCurrentWebDirtBeat webDirt = readTempo webDirt >>= beatNowWebDirt >>= return . toRational

clockedTickWebDirt :: T.JSVal -> (Tempo -> Int -> IO()) -> IO ()
clockedTickWebDirt webDirt callback = do
  nowBeat <- getCurrentWebDirtBeat webDirt
  let nextTick = ceiling (nowBeat * (fromIntegral webDirtTicksPerCycle))
  iterateM_ (clockedTickWebDirtLoop webDirt callback mTempo) nextTick

clockedTickWebDirtLoop webDirt callback tick = do
  tempo <- readTempo webDirt
  if (paused tempo)
    then do
      let pause = 0.01
      threadDelay $ floor (pause * 1000000)
      return $ if cps tempo < 0 then 0 else tick -- reset tick to 0 if cps is negative
    else do
      now <- liftM (posixSecondsToUTCTime . realToFrac) $ WebDirt.getCurrentTime webDirt
      let beatsFromAtToTick = fromIntegral tick / fromIntegral webDirtTicksPerCycle - beat tempo
          delayUntilTick = beatsFromAtToTick / cps tempo - realToFrac (diffUTCTime now (at tempo))
      threadDelay $ floor (delayUntilTick * 1000000)
      callback tempo tick
      return $ tick + 1

webDirtTick :: T.JSVal -> MVar ParamPattern -> Tempo -> Int -> IO ()
webDirtTick webDirt patternM tempo ticks = do
  p <- readMVar patternM
  let ticks' = (fromIntegral ticks) :: Integer
      a = ticks' % webDirtTicksPerCycle
      b = (ticks' + 1) % webDirtTicksPerCycle
      events = seqToRelOnsets (a,b) p -- :: [(Double,Map Param (Maybe Value))]
      events' = Prelude.map (\(o,m) -> (f o,m)) events
  E.catch (mapM_ (WebDirt.playSample webDirt) events') (\msg -> putStrLn $ "exception: " ++ show (msg :: E.SomeException))
  where f x = logicalOnset' tempo ticks x 0


{-

in Stream.hs:
type ToMessageFunc = Shape -> Tempo -> Int -> (Double, ParamMap) -> Maybe (IO ())

in OscStream.hs:
makeConnection :: String -> Int -> OscSlang -> IO (ToMessageFunc)
makeConnection returns a curried version of 'send' which uses logicalOnset

in Dirt.hs:
makeConnection is used in definition of dirtBackend :: IO Backend
the curried version of send is what is stored in the ToMessageFunc field of the Backend
in dirtStream, the backend is the first argument to stream

in Stream.hs:
stream :: Backend a -> Shape -> IO (ParamPattern -> IO ())
calls start (same args) which returns IO (MVar (ParamPattern)) (new mvar for pattern)
which forks clockedTick ticksPerCycle (onTick backend shape patternM)

in Tempo.hs:
clockedTick :: Int -> (Tempo -> Int -> IO()) -> IO ()
clockedTick basically just repeatedly calls the provided function with the
current tempo and tick

onTick :: Backend a -> Shape -> MVar ParamPattern -> Tempo -> Int -> IO ()
onTick    backend      shape    patternM             change   ticks
calls seqToRelOnsets (ticks%8,nextTick%8) thePattern
then mapMaybe (toMessage backend shape change ticks) on that...
in other words, it calls send :: Shape (shape) -> Tempo (change) -> Int (ticks)
 -> (Double,ParamMap) -> Maybe (IO ())

in OscStream.hs:
in the definition of send, logicalOnset is formed by calling
logicalOnset' change tick o ((latency shape) + nudge)

in Stream.hs:
logicalOnset' change tick o offset = logicalNow + (logicalPeriod * o) + offset
    where
      tpc = fromIntegral ticksPerCycle
      cycleD = ((fromIntegral tick) / tpc) :: Double
      -- i.e. current tick expressed in cycles
      logicalNow = logicalTime change cycleD
      -- i.e. POSIX time of current tick
      logicalPeriod = (logicalTime change (cycleD + (1/tpc))) - logicalNow
      -- POSIX time increment to next tick
      -- so overall return value is:
      -- POSIX time of current tick + o * duration of tick + an offset

in Tempo.hs:
logicalTime :: Tempo -> Double -> Double
given a tempo and a beat, returns the POSIX time of that beat

-}
