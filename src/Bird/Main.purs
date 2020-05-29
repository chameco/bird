module Bird.Main where

import Bird.Event (after, frames, key, keydown, keyup)
import Control.Bind (bind, discard, pure, when, (>>=))
import Control.Semigroupoid ((>>>))
import Data.Array (length, (!!))
import Data.DivisionRing (negate)
import Data.Eq ((==))
import Data.EuclideanRing (mod, (-), (/))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not, (&&))
import Data.Int (quot, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (max, min, (<), (<=), (>), (>=))
import Data.Semiring ((*), (+))
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (error, log)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, beginPath, clearRect, closePath, drawImageFull, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle, setLineWidth, setStrokeStyle, setTransform, stroke, translate, tryLoadImage, withContext)
import Web.HTML (window)
import Web.HTML.Window (outerHeight, outerWidth)

type Coords = {x :: Number, y :: Number}

at :: Number -> Number -> Coords
at x y = {x: x, y: y}

type Dims = {w :: Number, h :: Number}

by :: Number -> Number -> Dims
by w h = {w: w, h: h}

type Frame =
  { coords :: Coords
  , dims :: Dims
  , length :: Maybe Int
  }

type Animations =
  { defaultFrames :: Array Frame
  , modeFrames :: Map String (Array Frame)
  }

static :: Coords -> Dims -> Animations
static coords dims =
  { defaultFrames:
    [ { coords: coords
      , dims: dims
      , length: Nothing
      }
    ]
  , modeFrames: Map.empty 
  }

type Object =
  { texture :: CanvasImageSource
  , animations :: Animations
  , currentAnimation :: Maybe String
  , currentFrame :: Int
  , frameCounter :: Int
  , coords :: Coords
  , radius :: Number 
  , facingLeft :: Boolean
  }

buildObject ::
  String ->
  Animations ->
  Coords ->
  Number ->
  (Object -> Effect Unit) ->
  Effect Unit
buildObject path animations coords radius f = do
  tryLoadImage path $ \c -> case c of
    Nothing ->
      error $ fold
      [ "Failed to load texture at \""
      , path
      , "\""
      ]
    Just texture -> f
      { texture: texture
      , animations: animations
      , currentAnimation: Nothing
      , currentFrame: 0
      , frameCounter: 0
      , coords: coords
      , radius: radius
      , facingLeft: false 
      }

cloneObject :: Object -> Coords -> Object
cloneObject o coords = o
  { currentAnimation = Nothing
  , currentFrame = 0
  , frameCounter = 0
  , coords = coords
  }

objectCurrentFrames :: Object -> Array Frame
objectCurrentFrames o =
  case o.currentAnimation of
    Nothing -> o.animations.defaultFrames
    Just a -> case Map.lookup a o.animations.modeFrames of
      Nothing -> o.animations.defaultFrames
      Just fs -> fs

renderObject ::
  Context2D ->
  Ref Object ->
  Effect Unit
renderObject ctx ro = do
  o <- Ref.read ro
  let frames = objectCurrentFrames o
  let newCtr = o.frameCounter + 1
  let new = case frames !! o.currentFrame >>= \f -> f.length of
        Nothing -> o { frameCounter = newCtr }
        Just l ->
          if newCtr >= l
          then o { frameCounter = 0, currentFrame = mod (o.currentFrame + 1) $ length frames}
          else o { frameCounter = newCtr }
  Ref.write new ro
  case frames !! new.currentFrame of
    Nothing -> error "Object has no frame to draw"
    Just f -> do
      setTransform ctx {m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0}
      translate ctx {translateX: new.coords.x - f.dims.w / 2.0, translateY: new.coords.y - f.dims.h / 2.0}
      when new.facingLeft do
        scale ctx {scaleX: (-1.0), scaleY: 1.0}
        translate ctx {translateX: -f.dims.w, translateY: 0.0}
      drawImageFull ctx new.texture f.coords.x f.coords.y f.dims.w f.dims.h 0.0 0.0 f.dims.w f.dims.h

move :: Dims -> Dims -> Ref Object -> Effect Unit
move bounds dims = Ref.modify_ \o ->
  o
  { coords =
       { x: min bounds.w $ max 0.0 $ o.coords.x + dims.w
       , y: min bounds.h $ max 0.0 $ o.coords.y + dims.h
       }
  } 

gravity :: Number -> Ref Object -> Effect Unit
gravity floor ro = do
  Ref.modify_ (\o -> o { coords = {x: o.coords.x, y: min floor $ o.coords.y + 6.0 } }) ro
  o <- Ref.read ro
  when (o.coords.y >= floor) $ deanimate "falling" ro

faceLeft :: Ref Object -> Effect Unit
faceLeft = Ref.modify_ \o -> o { facingLeft = true } 

faceRight :: Ref Object -> Effect Unit
faceRight = Ref.modify_ \o -> o { facingLeft = false } 

flip :: Ref Object -> Effect Unit
flip = Ref.modify_ \o -> o { facingLeft = not o.facingLeft } 

animate :: String -> Ref Object -> Effect Unit
animate a = Ref.modify_ \o ->
  case o.currentAnimation of
    Just a' | a == a' -> o
    _ -> o { currentAnimation = Just a, currentFrame = 0, frameCounter = 0 }

deanimate :: String -> Ref Object -> Effect Unit
deanimate a = Ref.modify_ \o ->
  case o.currentAnimation of
    Just a' | a == a' -> o { currentAnimation = Nothing, currentFrame = 0, frameCounter = 0 }
    _ -> o

toggleAnimation :: String -> Ref Object -> Effect Unit
toggleAnimation a = Ref.modify_ \o ->
  case o.currentAnimation of
    Just a' | a == a' -> o { currentAnimation = Nothing, currentFrame = 0, frameCounter = 0 }
    Just _ -> o
    Nothing -> o { currentAnimation = Just a, currentFrame = 0, frameCounter = 0 }

renderFloor :: Context2D -> Number -> Number -> Effect Unit
renderFloor ctx w y = do
  setStrokeStyle ctx "black"
  setLineWidth ctx 2.0
  beginPath ctx
  moveTo ctx 0.0 y
  lineTo ctx w y
  closePath ctx
  stroke ctx

windowBounds :: CanvasElement -> Effect Dims
windowBounds canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  pure {w: width, h: height}

birdSheet :: Animations
birdSheet =
  { defaultFrames:
    [ {coords: at (256.0 * 0.0) 0.0, dims: walkingDims, length: Nothing}
    ]
  , modeFrames: Map.fromFoldable
    [ ( Tuple "walking"
        [ {coords: at (256.0 * 0.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 1.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 2.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 3.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 4.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 5.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 6.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 7.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 8.0) 0.0, dims: walkingDims, length: Just 3}
        , {coords: at (256.0 * 9.0) 0.0, dims: walkingDims, length: Just 3}
        ]
      )
    , ( Tuple "jumping"
        [ {coords: at (256.0 * 0.0) 256.0, dims: flyingDims, length: Nothing}
        ]
      )
    , ( Tuple "flying"
        [ {coords: at (512.0 * 0.0) 1024.0, dims: flyingDims, length: Just 5}
        , {coords: at (512.0 * 1.0) 1024.0, dims: flyingDims, length: Just 5}
        , {coords: at (512.0 * 2.0) 1024.0, dims: flyingDims, length: Just 5}
        , {coords: at (512.0 * 3.0) 1024.0, dims: flyingDims, length: Just 5}
        , {coords: at (512.0 * 4.0) 1024.0, dims: flyingDims, length: Just 5}
        , {coords: at (512.0 * 5.0) 1024.0, dims: flyingDims, length: Just 5}
        , {coords: at (512.0 * 6.0) 1024.0, dims: flyingDims, length: Just 5}
        ]
      )
    , ( Tuple "falling"
        [ {coords: at (512.0 * 1.0) 1280.0, dims: flyingDims, length: Nothing}
        ]
      )
    ]
  }
  where
    walkingDims = by 256.0 256.0
    flyingDims = by 512.0 256.0

main :: Effect Unit
main = do
  getCanvasElementById "canvas" >>= \c -> case c of
    Nothing -> error "Failed to find canvas element!"
    Just canvas -> do
      win <- window
      startWidth <- toNumber <$> outerWidth win
      startHeight <- toNumber <$> outerHeight win
      setCanvasWidth canvas startWidth
      setCanvasHeight canvas startHeight
      ctx <- getContext2D canvas
      buildObject "bird.png" birdSheet (at (startWidth / 2.0) (startHeight - 200.0)) 64.0 \archBird -> do
        bird <- Ref.new archBird
        isFlying <- Ref.new false
        key "ArrowLeft" do
          faceLeft bird
          flying <- Ref.read isFlying
          when (not flying) $ animate "walking" bird
          bounds <- windowBounds canvas
          move bounds (by (-5.0) 0.0) bird
        keyup "ArrowLeft" $ deanimate "walking" bird
        key "ArrowRight" do
          faceRight bird
          flying <- Ref.read isFlying
          when (not flying) $ animate "walking" bird
          bounds <- windowBounds canvas
          move bounds (by 5.0 0.0) bird
        keyup "ArrowRight" $ deanimate "walking" bird
        key " " do
          animate "flying" bird
          Ref.write true isFlying
          bounds <- windowBounds canvas
          move bounds (by 0.0 (-13.0)) bird
        keyup " " do
          Ref.write false isFlying
          deanimate "flying" bird
        frames $ withContext ctx do
          width <- getCanvasWidth canvas
          height <- getCanvasHeight canvas

          gravity (height - 200.0) bird
          b <- Ref.read bird
          flying <- Ref.read isFlying
          when (not flying && b.coords.y < height - 200.0) $ animate "falling" bird

          clearRect ctx {x: 0.0, y: 0.0, width: width, height: height}
          renderFloor ctx width $ height - 200.0
          renderObject ctx bird
