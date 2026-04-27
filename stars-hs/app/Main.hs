{-|
Module: stars-hs
Description: 'stars' test project haskell implementation.
-}

module Main where

import Control.Monad
import Control.Category hiding (id)
import Data.Word
import Data.Bits
import Data.Ord
import Data.List
import Data.Bifunctor
import Foreign.Storable
import Foreign.Ptr
import Foreign.Marshal
import Data.Text (pack)
import SDL hiding (Timer)
import qualified SDL.Raw as SR

data RandomState = RandomState Word64 Word64 Word64 Word64

rsInit :: Word64 -> RandomState
rsInit seed0 = RandomState (head sm) (sm !! 1) (sm !! 2) (sm !! 3) where
  sm = splitmix64 seed0
  splitmix64 seed = (r2 .^. (r2 .>>. 31)) : splitmix64 r0 where
    r0 = seed + 0x9E3779B97F4A7C15
    r1 = (r0 .^. (r0 .>>. 30)) * 0xBF58476D1CE4E5B9
    r2 = (r1 .^. (r1 .>>. 27)) * 0x94D049BB133111EB

rsNextWord64 :: RandomState -> (Word64, RandomState)
rsNextWord64 (RandomState v0 v1 v2 v3) = let
  v2' = v2 .^. v0
  v3' = v3 .^. v1
  v1' = v1 .^. v2'
  v0' = v0 .^. v3'
  v2'' = v2' .^. (v1 .<<. 17)
  v3'' = rotateL v3' 45
  in (v0 + rotateL (v3 + v0) 23, RandomState v0' v1' v2'' v3'')

rsNextFloat :: RandomState -> (Float, RandomState)
rsNextFloat rs = (fromIntegral rw / fromIntegral (maxBound :: Word64), rs') where
  (rw, rs') = rsNextWord64 rs

rsNextUnitVec3 :: RandomState -> (Vec3 Float, RandomState)
rsNextUnitVec3 rs0 = (v, rs2) where
  (p, rs1) = rsNextFloat rs0
  (t, rs2) = rsNextFloat rs1
  v = vFromSpherical (2 * pi * p) (acos (t * 2 - 1))

rsNextSphereVec3 :: RandomState -> (Vec3 Float, RandomState)
rsNextSphereVec3 rs0 = if vDot v v <= 1 then (v, rs3) else rsNextSphereVec3 rs3 where
  (x, rs1) = rsNextFloat rs0
  (y, rs2) = rsNextFloat rs1
  (z, rs3) = rsNextFloat rs2
  v = (\c -> c * 2 - 1) <$> Vec3 x y z

rsNextSeq :: RandomState -> Int -> (RandomState -> (a, RandomState)) -> ([a], RandomState)
rsNextSeq rs steps gen = impl steps ([], rs) where
  impl n (s, rs0) = if n == 0 then (s, rs0) else impl (n - 1) (first (:s) (gen rs0))

data Vec3 a = Vec3 a a a

vSplat :: a -> Vec3 a
vSplat c = Vec3 c c c

vDot :: Num a => Vec3 a -> Vec3 a -> a
vDot (Vec3 x0 y0 z0) (Vec3 x1 y1 z1) = x0 * x1 + y0 * y1 + z0 * z1

vFromSpherical :: Float -> Float -> Vec3 Float
vFromSpherical phi theta = Vec3 (pc * ts) (ps * ts) tc where
  (ps, pc, ts, tc) = (sin phi, cos phi, sin theta, cos theta)

vZip :: (a -> a -> a) -> Vec3 a -> Vec3 a -> Vec3 a
vZip f (Vec3 lx ly lz) (Vec3 rx ry rz) = Vec3 (f lx rx) (f ly ry) (f lz rz)

instance Functor Vec3 where
  fmap f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

instance Num a => Num (Vec3 a) where
  (+) = vZip (+)
  (*) = vZip (*)
  abs = fmap abs
  signum = fmap signum
  negate = fmap negate
  fromInteger i = vSplat (fromInteger i)

data Timer = Timer
  { tStart :: Word64
  , tNow :: Word64
  , tFreq :: Word64
  , tDeltaTime :: Float
  , tTime :: Float
  , tFpsFrameCount :: Int
  , tFpsDuration :: Word64
  , tFpsLastMeasure :: Word64
  , tFps :: Float
  }

tNewFps :: Timer -> Maybe Float
tNewFps t = if tFpsLastMeasure t == tNow t then Just (tFps t) else Nothing

tInit :: Word64 -> Word64 -> Timer
tInit freq now = Timer
  { tStart = now
  , tNow = now
  , tFreq = freq
  , tDeltaTime = 0.01
  , tTime = 0.00
  , tFpsFrameCount = 0
  , tFpsDuration = 3 * freq
  , tFpsLastMeasure = now - 1
  , tFps = 0
  }

tUpdate :: Timer -> Word64 -> Timer
tUpdate t now = t' where
  dt start end = fromIntegral (end - start) / fromIntegral (tFreq t)
  (fps, fc, lm) = if now - tFpsLastMeasure t <= tFpsDuration t
    then (tFps t, tFpsFrameCount t + 1, tFpsLastMeasure t)
    else (fromIntegral (tFpsFrameCount t) / dt (tFpsLastMeasure t) now, 0, now)
  t' = t
    { tNow = now
    , tDeltaTime = dt (tNow t) now
    , tTime = dt (tStart t) now
    , tFps = fps
    , tFpsFrameCount = fc
    , tFpsLastMeasure = lm
    }

data Input = Input
  { iRot :: Float
  , iAcc :: Float
  , iDx :: Float
  , iDy :: Float
  }

iZero :: Input
iZero = Input 0 0 0 0

iCompose :: Input -> Input -> Input
iCompose li ri = Input
  { iRot = cas iRot
  , iAcc = cas iAcc
  , iDx = cas iDx
  , iDy = cas iDy
  } where cas f = clamp (-1, 1) (f li + f ri)

data Context = Context
  { cWindow :: Window
  , cStars :: [Vec3 Float]
  , cRand :: RandomState
  , cTimer :: Timer
  , cInput :: Input
  , cSpeed :: Float
  }

rotateStars :: Float -> [Vec3 Float] -> [Vec3 Float]
rotateStars a stars = rot <$> stars where
  rot (Vec3 x y z) = Vec3 (z * s + x * c) y (z * c - x * s)
  (s, c) = (sin a, cos a)
  
moveStars :: RandomState -> Vec3 Float -> [Vec3 Float] -> ([Vec3 Float], RandomState)
moveStars rs delta stars = (stars' ++ nstars', rs') where
  stars' = fmap (+delta) >>> filter (\v -> vDot v v <= 1) $ stars
  (nstars, rs') = rsNextSeq rs (length stars - length stars') rsNextUnitVec3
  nstars' = fmap (\v -> v * vSplat (signum (-vDot v delta))) nstars

cHandleEvents :: [Event] -> Input -> Maybe Input
cHandleEvents [] inp = Just inp
cHandleEvents (evt:rest) inp = case eventPayload evt of
  QuitEvent -> Nothing
  KeyboardEvent d -> cHandleEvents rest inp'' where
    ks = case keyboardEventKeyMotion d of
      Pressed -> 1
      Released -> -1
    inp' = case keysymScancode (keyboardEventKeysym d) of
      ScancodeQ -> iZero { iRot =  ks }
      ScancodeE -> iZero { iRot = -ks }
      ScancodeR -> iZero { iAcc =  ks }
      ScancodeF -> iZero { iAcc = -ks }
      ScancodeW -> iZero { iDy  =  ks }
      ScancodeS -> iZero { iDy  = -ks }
      ScancodeD -> iZero { iDx  =  ks }
      ScancodeA -> iZero { iDx  = -ks }
      _ -> iZero
    inp'' = if keyboardEventRepeat d then inp else iCompose inp' inp
  _ -> cHandleEvents rest inp

cUpdate :: Context -> IO (Maybe Context)
cUpdate ctx = do
  events <- pollEvents
  newTime <- SR.getPerformanceCounter
  return $ do
    input' <- cHandleEvents events (cInput ctx)
    let
      rotationSpeed = 1
      accelerationSpeed = 1
      timer' = tUpdate (cTimer ctx) newTime
      speed' = cSpeed ctx + tDeltaTime timer' * iAcc input' * accelerationSpeed

      rot = if abs (iRot input') > 0.1
        then rotateStars (iRot input' * rotationSpeed * tDeltaTime timer')
        else id
      sd = Vec3 (iDx input') (iDy input') 1 * vSplat ((-speed') * tDeltaTime timer')
      (stars', rand') = first rot $ moveStars (cRand ctx) sd (cStars ctx)

    return $ ctx
      { cInput = input'
      , cTimer = timer'
      , cStars = stars'
      , cRand = rand'
      , cSpeed = speed'
      }

cRender :: Context -> IO ()
cRender ctx = do
  case tNewFps (cTimer ctx) of
    Just fps -> putStrLn $ "FPS: " ++ show fps
    _ -> return ()

  surface <- getWindowSurface (cWindow ctx)
  (SurfacePixelFormat fptr) <- surfaceFormat surface
  format <- peek fptr

  when (SR.pixelFormatFormat format == SR.SDL_PIXELFORMAT_RGB888) $ do
    V2 cSurfaceW cSurfaceH <- surfaceDimensions surface

    let
      surfaceW = fromIntegral cSurfaceW :: Int
      surfaceH = fromIntegral cSurfaceH :: Int
      surfaceBPP = fromIntegral (SR.pixelFormatBytesPerPixel format)
      surfacePitch = surfaceW * surfaceBPP

      clip = 0.5 :: Float
      w = fromIntegral surfaceW
      h = fromIntegral surfaceH
      halfW = w / 2.0 :: Float
      halfH = h / 2.0 :: Float
      whScale = sqrt ((w * w + h * h) / (1 - clip * clip))
      xyMul = clip * w * h / whScale

      mkProjBuffer = id
        >>> filter (\(Vec3 _ _ z) -> z > 0)
        >>> map (\v@(Vec3 x y z) -> (
                    truncate (halfW + xyMul * x / z) :: Int,
                    truncate (halfH - xyMul * y / z) :: Int,
                    vDot v v))
        >>> filter (\(x, y, _) -> x >= 0 && x <= surfaceW - 4 
                     && y >= 0 && y <= surfaceH - 4)
        >>> sortBy (\(_, _, lz) (_, _, rz) -> compare rz lz)

    lockSurface surface

    surfacePtr <- surfacePixels surface
    fillBytes surfacePtr 0 (surfaceH * surfacePitch)
    forM_ (mkProjBuffer (cStars ctx)) $ \(xs, ys, d) -> do
      let
        pixelPtr = plusPtr surfacePtr (ys * surfacePitch + xs * surfaceBPP)
        size | d < 0.0025 = 4 :: Int
             | d < 0.01 = 3
             | d < 0.09 = 2
             | otherwise = 1
        color = round (255 * (1 - d)) :: Word8

      forM_ [0..size - 1] $ \y -> do
        fillBytes (plusPtr pixelPtr (y * surfacePitch)) color (size * surfaceBPP)

    unlockSurface surface

    updateWindowSurface (cWindow ctx)

cRun :: Context -> IO ()
cRun ctx0 = do
  ctx <- cUpdate ctx0
  case ctx of
    Just c -> do
      cRender c
      cRun c
    _ -> return ()

main :: IO ()
main = do
  window <- createWindow (pack "stars-hs") defaultWindow
  timer <- tInit
    <$> SR.getPerformanceFrequency
    <*> SR.getPerformanceCounter

  let (stars, rand) = rsNextSeq (rsInit 47) 8192 rsNextSphereVec3
  cRun Context
    { cWindow = window
    , cStars = stars
    , cRand = rand
    , cTimer = timer
    , cInput = iZero
    , cSpeed = 0.47
    }
