-- STARS-HS project main module

module Main (main) where

import Control.Monad (when, unless)
import Control.Monad.ST (runST)
import Data.Bits
import Data.Foldable (traverse_)
import Data.Function
import Data.Int (Int32)
import Data.Ord (clamp)
import Data.Word (Word8, Word64)
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Text.Printf (printf)
import qualified Data.Text as Text
import qualified Data.Vector.Algorithms.Intro as VecSort
import qualified Data.Vector.Storable as Vec
import qualified SDL
import qualified SDL.Raw

-- Xoshiro256++ based random number generator current state
data RandomState = RandomState Word64 Word64 Word64 Word64

-- Initialize random state
randomInit :: Word64 -> RandomState
randomInit seed = RandomState (head spm) (spm !! 1) (spm !! 2) (spm !! 3) where
    spm = splitMix64 seed

    -- Splitmix64 random number generator
    splitMix64 :: Word64 -> [Word64]
    splitMix64 x0 = r2:splitMix64 x1 where
        x1 = x0 + 0x9E3779B97F4A7C15
        r0 = (x1 .^. (x1 .>>. 30)) * 0xBF58476D1CE4E5B9
        r1 = (r0 .^. (r0 .>>. 27)) * 0x94D049BB133111EB
        r2 =  r1 .^. (r1 .>>. 31)

-- Generate random unsigned 64-bit integer
randomInt64 :: RandomState -> (Word64, RandomState)
randomInt64 (RandomState s0 s1 s2 s3) = (s0 + rotateL (s0 + s3) 23, RandomState s0' s1' s2'' s3'') where
    s2' = s2 .^. s0
    s3' = s3 .^. s1
    s1' = s1 .^. s2'
    s0' = s0 .^. s3'
    s2'' = s2' .^. (s1 .<<. 17)
    s3'' = rotateL s3' 45

-- Generate random float in [0..1] range
randomUnitFloat :: RandomState -> (Float, RandomState)
randomUnitFloat gen = (realToFrac double, gen') where
    (int, gen') = randomInt64 gen
    double = (fromIntegral int :: Double) / (fromIntegral (maxBound :: Word64) :: Double)

    -- On-screen 3-component vector
data ScreenVec = ScreenVec Int Int Float

instance Storable ScreenVec where
    sizeOf _ = 12
    alignment _ = 4

    peek ptr = ScreenVec
        <$> fmap fromIntegral (peekElemOff (castPtr ptr :: Ptr Int32) 0)
        <*> fmap fromIntegral (peekElemOff (castPtr ptr :: Ptr Int32) 1)
        <*> peekElemOff (castPtr ptr :: Ptr Float) 2

    poke ptr (ScreenVec x y z) = do
        pokeElemOff (castPtr ptr :: Ptr Int32) 0 (fromIntegral x)
        pokeElemOff (castPtr ptr :: Ptr Int32) 1 (fromIntegral y)
        pokeElemOff (castPtr ptr :: Ptr Float) 2 z

-- 3-component vector
data Vec3 = Vec3 Float Float Float

-- Make Vec3 storable
instance Storable Vec3 where
    sizeOf _ = 12
    alignment _ = 4

    peek ptr = Vec3
        <$> peekElemOff cptr 0
        <*> peekElemOff cptr 1
        <*> peekElemOff cptr 2
            where cptr = castPtr ptr :: Ptr Float

    poke ptr (Vec3 x y z) = do
        let cptr = castPtr ptr :: Ptr Float
        pokeElemOff cptr 0 x
        pokeElemOff cptr 1 y
        pokeElemOff cptr 2 z

-- Implement basic vector operations
instance Num Vec3 where
    (+) = vec3Zip (+)
    (*) = vec3Zip (*)
    abs = vec3Map abs
    signum = vec3Map signum
    negate = vec3Map negate
    fromInteger = vec3FromFloat . fromInteger

-- Vec3 from single float
vec3FromFloat :: Float -> Vec3
vec3FromFloat f = Vec3 f f f

-- Map 3-component vector
vec3Map :: (Float -> Float) -> Vec3 -> Vec3
vec3Map f (Vec3 x y z) = Vec3 (f x) (f y) (f z)

-- Zip two 3-component vectors with some operator
vec3Zip :: (Float -> Float -> Float) -> Vec3 -> Vec3 -> Vec3
vec3Zip f (Vec3 lx ly lz) (Vec3 rx ry rz) = Vec3 (f lx rx) (f ly ry) (f lz rz)

-- 3-component vector dot product
vec3Dot :: Vec3 -> Vec3 -> Float
vec3Dot (Vec3 lx ly lz) (Vec3 rx ry rz) = lx * rx + ly * ry + lz * rz

-- Make Vec3 from spherical coordinates
vec3FromSpherical :: Float -> Float -> Vec3
vec3FromSpherical phi theta = Vec3 (cos phi * sin theta) (sin phi * sin theta) (cos theta)

-- Random vector with uniform distribution **on** unit sphere
randomUnitVec3 :: RandomState -> (Vec3, RandomState)
randomUnitVec3 gen0 = (vec3FromSpherical (2 * pi * ksi2) (acos (2 * ksi1 - 1)), gen2) where
    (ksi1, gen1) = randomUnitFloat gen0
    (ksi2, gen2) = randomUnitFloat gen1

-- Random vector with uniform distribution **in** unit sphere
randomSphereVec3 :: RandomState -> (Vec3, RandomState)
randomSphereVec3 gen0 = result where
    (x, gen1) = randomUnitFloat gen0
    (y, gen2) = randomUnitFloat gen1
    (z, gen3) = randomUnitFloat gen2
    v = vec3Map (\c -> c * 2 - 1) (Vec3 x y z)
    result = if vec3Dot v v <= 1.0
        then (v, gen3)
        else randomSphereVec3 gen3

-- Time controller (time, deltaTime and FPS counter)
data Timer = Timer
    { timerFrequency      :: Word64
    , timerStart          :: Word64
    , timerNow            :: Word64
    , timerDeltaTime      :: Float
    , timerTime           :: Float
    , timerFpsFrameCount  :: Word64
    , timerFpsDuration    :: Word64
    , timerFpsLastMeasure :: Word64
    , timerFps            :: Float
    , timerFpsIsNew       :: Bool
    }

-- Construct timer
timerInit :: Word64 -> Word64 -> Timer
timerInit performanceCounter performanceFrequency = Timer
    { timerFrequency = performanceFrequency
    , timerStart = performanceCounter
    , timerNow = performanceCounter
    , timerDeltaTime = 0.01
    , timerTime = 0.0
    , timerFpsFrameCount = 0
    , timerFpsDuration = performanceFrequency * 3
    , timerFpsLastMeasure = performanceCounter
    , timerFps = 0
    , timerFpsIsNew = False
    }

-- Update timer with new performanceCounter value
timerUpdate :: Timer -> Word64 -> Timer
timerUpdate timer performanceCounter = timer' where
    getFloatDuration :: Word64 -> Word64 -> Float
    getFloatDuration begin end = realToFrac (delta / norm) :: Float where
        delta = fromIntegral (end - begin) :: Double
        norm = fromIntegral (timerFrequency timer) :: Double

    deltaTime = getFloatDuration (timerNow timer) performanceCounter
    time = getFloatDuration (timerStart timer) performanceCounter

    fpsUpdateRequired = performanceCounter - timerFpsLastMeasure timer > timerFpsDuration timer
    (fps, fpsFrameCount, fpsLastMeasure) = if fpsUpdateRequired
        then let
                duration = getFloatDuration (timerFpsLastMeasure timer) performanceCounter
                newFps = (fromIntegral (timerFpsFrameCount timer) :: Float) / duration
            in
                (newFps, 0, performanceCounter)
        else (timerFps timer, 1 + timerFpsFrameCount timer, timerFpsLastMeasure timer)

    timer' = timer
        { timerNow = performanceCounter
        , timerDeltaTime = deltaTime
        , timerTime = time
        , timerFpsFrameCount = fpsFrameCount
        , timerFpsLastMeasure = fpsLastMeasure
        , timerFps = fps
        , timerFpsIsNew = fpsUpdateRequired
        }

-- Context structure
data Context = Context
    { contextWindow :: SDL.Window
    , contextStars  :: Vec.Vector Vec3
    , contextRandom :: RandomState
    , contextInput  :: InputAxis
    , contextSpeed  :: Float
    , contextTimer  :: Timer
    }

-- Rendering function
render :: Context -> IO ()
render context = do
    surface <- SDL.getWindowSurface (contextWindow context)
    SDL.SurfacePixelFormat formatPtr <- SDL.surfaceFormat surface
    pixelFormat <- peek formatPtr

    when (SDL.Raw.pixelFormatFormat pixelFormat == SDL.Raw.SDL_PIXELFORMAT_RGB888) $ do
        SDL.V2 cSurfaceW cSurfaceH <- SDL.surfaceDimensions surface
        SDL.lockSurface surface
        surfacePixels <- SDL.surfacePixels surface

        let
            surfaceW = fromIntegral cSurfaceW
            surfaceH = fromIntegral cSurfaceH
            surfaceBPP = fromIntegral (SDL.Raw.pixelFormatBytesPerPixel pixelFormat)
            surfacePitch = surfaceW * surfaceBPP

            clip = 0.5 :: Float
            wFloat = fromIntegral surfaceW :: Float
            hFloat = fromIntegral surfaceH :: Float
            halfW = wFloat / 2
            halfH = hFloat / 2
            whScale = sqrt ((wFloat * wFloat + hFloat * hFloat) / (1 - clip * clip))
            xyMul = clip * wFloat * hFloat / whScale

            -- Generate star coordinate vector
            starCoords :: Vec.Vector Vec3 -> Vec.Vector ScreenVec
            starCoords =
                (\stars -> runST $ do
                    mutStars <- Vec.thaw stars
                    VecSort.sortBy (\(ScreenVec _ _ dl2) (ScreenVec _ _ dr2) -> compare dr2 dl2) mutStars
                    Vec.freeze mutStars
                )
                . Vec.filter (\(ScreenVec xs ys _) -> xs >= 0 && ys >= 0 && xs <= surfaceW - 4 && ys <= surfaceH - 4)
                . Vec.map (\(Vec3 x y z) -> ScreenVec
                    (round (halfW + xyMul * x / z))
                    (round (halfH - xyMul * y / z))
                    (x * x + y * y + z * z)
                )
                . Vec.filter (\(Vec3 _ _ z) -> z > 0)

            -- Render single star
            renderStar :: ScreenVec -> IO ()
            renderStar (ScreenVec xs ys d2) = drawBox size basePtr where
                basePtr = plusPtr surfacePixels (ys * surfacePitch + xs * 4)

                size | d2 < 0.0025 = 4
                     | d2 < 0.01 = 3
                     | d2 < 0.09 = 2
                     | otherwise = 1
                color = round (255 * (1 - d2)) :: Word8

                drawBox :: Int -> Ptr () -> IO ()
                drawBox count ptr = when (count /= 0) $ do
                    fillBytes ptr color (size * 4)
                    drawBox (count - 1) (plusPtr ptr surfacePitch)

        -- Clear screen and render stars
        fillBytes surfacePixels 0x00 (surfacePitch * surfaceH)
        traverse_ renderStar (Vec.toList (starCoords (contextStars context)))

        SDL.unlockSurface surface

    SDL.updateWindowSurface (contextWindow context)

moveStars :: RandomState -> Vec3 -> Vec.Vector Vec3 -> (Vec.Vector Vec3, RandomState)
moveStars random delta stars = (stars'', random') where

    -- Random star generator
    genRandomStar :: RandomState -> (Vec3, RandomState)
    genRandomStar rand = (item', rand') where
        (item, rand') = randomUnitVec3 rand
        item' = item * (vec3FromFloat . negate . signum $ vec3Dot item delta)

    starCount = Vec.length stars
    stars' = Vec.filter (\s -> vec3Dot s s <= 1.0) . Vec.map (+ delta) $ stars

    (newStars, random') = genRandomSeq genRandomStar (starCount - Vec.length stars') random
    stars'' = stars' Vec.++ Vec.fromList newStars

rotateStarsY :: Float -> Vec.Vector Vec3 -> Vec.Vector Vec3
rotateStarsY angle = Vec.map rotateStar where
    sinA = sin angle
    cosA = cos angle
    rotateStar :: Vec3 -> Vec3
    rotateStar (Vec3 x y z) = Vec3 (z * sinA + x * cosA) y (z * cosA - x * sinA)

data InputAxis = InputAxis
    { inputAxisRotation     :: Float
    , inputAxisAcceleration :: Float
    , inputAxisMoveX        :: Float
    , inputAxisMoveY        :: Float
    }

inputAxisZero :: InputAxis
inputAxisZero = InputAxis
    { inputAxisRotation = 0
    , inputAxisAcceleration = 0
    , inputAxisMoveX = 0
    , inputAxisMoveY = 0
    }

-- Compose two inputs
inputAxisCompose :: InputAxis -> InputAxis -> InputAxis
inputAxisCompose prev next = InputAxis
    { inputAxisRotation     = clampAxisSum inputAxisRotation
    , inputAxisAcceleration = clampAxisSum inputAxisAcceleration
    , inputAxisMoveX        = clampAxisSum inputAxisMoveX
    , inputAxisMoveY        = clampAxisSum inputAxisMoveY
    }
    where
        clampAxisSum :: (InputAxis -> Float) -> Float
        clampAxisSum comp = clamp (-1, 1) (comp prev + comp next)

-- Update context for next frame
updateContext :: Context -> InputAxis -> Word64 -> Context
updateContext context inputDelta performanceCounter = context' where
    rotationSpeed = 1 :: Float
    accelerationSpeed = 1 :: Float
    input = inputAxisCompose (contextInput context) inputDelta
    timer = timerUpdate (contextTimer context) performanceCounter

    deltaTime = timerDeltaTime timer
    moveSpeed = contextSpeed context + deltaTime * inputAxisAcceleration input * accelerationSpeed
    starDelta = Vec3 (inputAxisMoveX input) (inputAxisMoveY input) 1 * vec3FromFloat (-(moveSpeed * deltaTime))

    (stars', random) = moveStars (contextRandom context) starDelta
        . applyWhen (abs (inputAxisRotation input) >= 0.1) (rotateStarsY (rotationSpeed * deltaTime * inputAxisRotation input))
        . contextStars $ context

    context' = Context
        { contextWindow = contextWindow context
        , contextStars = stars'
        , contextRandom = random
        , contextInput = input
        , contextSpeed = moveSpeed
        , contextTimer = timer
        }

-- Handle array of the input events
handleEvents :: [SDL.Event] -> InputAxis -> (Bool, InputAxis)
handleEvents [] input = (False, input)
handleEvents (event : restEvents) input = (eQuit || quit', input') where
    (eQuit, eInput) = case SDL.eventPayload event of
        SDL.QuitEvent -> (True, inputAxisZero)

        SDL.KeyboardEvent keyboardEvent -> (False, inputAxis) where
            keycode = SDL.keysymKeycode (SDL.keyboardEventKeysym keyboardEvent)
            isPressed = SDL.keyboardEventKeyMotion keyboardEvent == SDL.Pressed
            isRepeat = SDL.keyboardEventRepeat keyboardEvent

            delta | isRepeat = 0
                  | isPressed = 1
                  | otherwise = -1

            inputAxis = case keycode of
                SDL.KeycodeQ -> inputAxisZero { inputAxisRotation     =  delta }
                SDL.KeycodeE -> inputAxisZero { inputAxisRotation     = -delta }
                SDL.KeycodeR -> inputAxisZero { inputAxisAcceleration =  delta }
                SDL.KeycodeF -> inputAxisZero { inputAxisAcceleration = -delta }
                SDL.KeycodeW -> inputAxisZero { inputAxisMoveY        =  delta }
                SDL.KeycodeS -> inputAxisZero { inputAxisMoveY        = -delta }
                SDL.KeycodeD -> inputAxisZero { inputAxisMoveX        =  delta }
                SDL.KeycodeA -> inputAxisZero { inputAxisMoveX        = -delta }
                _ -> inputAxisZero

        _ -> (False, inputAxisZero)

    (quit', input') = handleEvents restEvents (inputAxisCompose input eInput)

-- Main loop function
mainLoop :: Context -> IO ()
mainLoop context = do
    events <- SDL.pollEvents
    let (doQuit, inputDelta) = handleEvents events inputAxisZero

    -- Continue if not quit
    unless doQuit $ do
        performanceCounter <- SDL.Raw.getPerformanceCounter
        let newCtx = updateContext context inputDelta performanceCounter

        when (timerFpsIsNew (contextTimer newCtx)) (printf "FPS: %f\n" (timerFps (contextTimer newCtx)))

        render newCtx
        mainLoop newCtx

-- Generate random finite sequence of some items
genRandomSeq :: (RandomState -> (a, RandomState)) -> Int -> RandomState -> ([a], RandomState)
genRandomSeq gen = impl where
    impl 0 rnd = ([], rnd)
    impl count rnd0 = (item : rest, rnd2) where
        (item, rnd1) = gen rnd0
        (rest, rnd2) = impl (count - 1) rnd1

main :: IO ()
main = do
    -- Initialize SDL and create window
    SDL.initialize [SDL.InitVideo]
    window <- SDL.createWindow (Text.pack "stars-hs") SDL.defaultWindow

    -- Initialize timer
    timer <- timerInit
        <$> SDL.Raw.getPerformanceCounter
        <*> SDL.Raw.getPerformanceFrequency

    -- Construct stars and initial random state
    let (stars, random) = genRandomSeq randomSphereVec3 8192 (randomInit 47)

    -- Run!
    mainLoop (Context
        { contextWindow = window
        , contextStars = Vec.fromList stars
        , contextRandom = random
        , contextInput = inputAxisZero
        , contextSpeed = 0.47
        , contextTimer = timer
        })

    -- Destroy window and deinitialize SDL
    SDL.destroyWindow window
    SDL.quit
