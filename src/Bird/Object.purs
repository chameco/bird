module Bird.Object where

import Bird.Event (setCanvasBackground)
import Control.Bind (bind, discard, pure, when, (>>=))
import Data.Array (filter, head, length, (!!))
import Data.Eq ((==))
import Data.EuclideanRing (mod, (/))
import Data.Foldable (fold)
import Data.Function (($))
import Data.HeytingAlgebra (not, (&&))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (max, min, (<), (<=), (>), (>=))
import Data.Ring (negate, (-))
import Data.Semiring ((*), (+))
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Console (error)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImageFull, scale, setCanvasHeight, setCanvasWidth, setTransform, translate, tryLoadImage)

type Coords = {x :: Number, y :: Number}

at :: Number -> Number -> Coords
at x y = {x: x, y: y}

type Dims = {w :: Number, h :: Number}

by :: Number -> Number -> Dims
by w h = {w: w, h: h}

overlaps ::
  forall a b.
  {coords :: Coords, dims :: Dims | a} ->
  {coords :: Coords, dims :: Dims | b} ->
  Boolean
overlaps a b =
  a.coords.x < b.coords.x + b.dims.w
  && a.coords.x + a.dims.w > b.coords.x
  && a.coords.y < b.coords.y + b.dims.h
  && a.coords.y + a.dims.h > b.coords.y

type Frame =
  { coords :: Coords
  , dims :: Dims
  , length :: Maybe Number
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
  , timeCounter :: Number
  , coords :: Coords
  , dims :: Dims 
  , facingLeft :: Boolean
  , solid :: Boolean
  }

buildObject ::
  String ->
  Animations ->
  Coords ->
  Dims ->
  Boolean ->
  (Object -> Effect Unit) ->
  Effect Unit
buildObject path animations coords dims solid f = do
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
      , timeCounter: 0.0
      , coords: coords
      , dims: dims
      , facingLeft: false 
      , solid: solid
      }

cloneObject :: Object -> Coords -> Object
cloneObject o coords = o
  { currentAnimation = Nothing
  , currentFrame = 0
  , timeCounter = 0.0
  , coords = coords
  }

objectCurrentFrames :: Object -> Array Frame
objectCurrentFrames o =
  case o.currentAnimation of
    Nothing -> o.animations.defaultFrames
    Just a -> case Map.lookup a o.animations.modeFrames of
      Nothing -> o.animations.defaultFrames
      Just fs -> fs

render ::
  Context2D ->
  Number ->
  Ref Object ->
  Effect Unit
render ctx delta ro = do
  o <- Ref.read ro
  let frames = objectCurrentFrames o
  let newCtr = o.timeCounter + delta
  let new = case frames !! o.currentFrame >>= \f -> f.length of
        Nothing -> o { timeCounter = newCtr }
        Just l ->
          if newCtr >= l
          then o { timeCounter = 0.0, currentFrame = mod (o.currentFrame + 1) $ length frames}
          else o { timeCounter = newCtr }
  Ref.write new ro
  case frames !! new.currentFrame of
    Nothing -> error "Object has no frame to draw"
    Just f -> do
      let x = new.coords.x + new.dims.w / 2.0
      let y = new.coords.y + new.dims.h / 2.0
      setTransform ctx {m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0}
      translate ctx {translateX: x - f.dims.w / 2.0, translateY: y - f.dims.h / 2.0}
      when new.facingLeft do
        scale ctx {scaleX: (-1.0), scaleY: 1.0}
        translate ctx {translateX: -f.dims.w, translateY: 0.0}
      drawImageFull ctx new.texture f.coords.x f.coords.y f.dims.w f.dims.h 0.0 0.0 f.dims.w f.dims.h

type Door =
  { coords :: Coords
  , dims :: Dims
  , toArea :: String
  , toCoords :: Dims -> Coords -> Coords
  }

type Area =
  { name :: String
  , background :: String
  , dims :: Dims
  , scaling :: Number
  , floorHeight :: Number
  , terrain :: Array Object
  , doors :: Array Door
  }

type WorldMap = Map String Area

leftEdge :: Dims -> Number -> String -> Door
leftEdge a s target =
  { coords: at 0.0 0.0
  , dims: by 3.0 $ a.h * s
  , toArea: target
  , toCoords: \d c -> c { x = d.w - 128.0 - 4.0}
  }

rightEdge :: Dims -> Number -> String -> Door
rightEdge a s target =
  { coords: at (a.w * s - 3.0) 0.0
  , dims: by 3.0 $ a.h * s
  , toArea: target
  , toCoords: \d c -> c { x = 4.0 }
  }

areaDims :: Area -> Dims
areaDims a = by (a.dims.w * a.scaling) (a.dims.h * a.scaling)

areaFloor :: Area -> Number
areaFloor a = (areaDims a).h - a.floorHeight * a.scaling

activateArea :: CanvasElement -> Area -> Effect Unit
activateArea canvas a = do
  setCanvasBackground a.background a.dims.w a.dims.h
  setCanvasWidth canvas (areaDims a).w
  setCanvasHeight canvas (areaDims a).h

warpObject :: Area -> Coords -> Object -> Object
warpObject a coords o =
  o
  { coords =
       { x: min ((areaDims a).w - o.dims.w) $ max 0.0 coords.x
       , y: min (areaFloor a - o.dims.h) $ max 0.0 coords.y
       }
  } 

moveObject :: Area -> Dims -> Object -> Object
moveObject a dims o =
  o
  { coords =
       { x: min ((areaDims a).w - o.dims.w) $ max 0.0 $ o.coords.x + dims.w
       , y: min (areaFloor a - o.dims.h) $ max 0.0 $ o.coords.y + dims.h
       }
  } 

warp :: Ref Area -> Coords -> Ref Object -> Effect Unit
warp ra coords ro = do
  a <- Ref.read ra
  Ref.modify_ (warpObject a coords) ro

move :: CanvasElement -> WorldMap -> Ref Area -> Dims -> Ref Object -> Effect Unit
move canvas world ra dims ro = do
  a <- Ref.read ra
  o <- Ref.read ro
  let new = moveObject a dims o
  case head (filter (overlaps new) a.doors) of
    Just door
      | Just newArea <- Map.lookup door.toArea world
        -> do
          Ref.write newArea ra
          activateArea canvas newArea
          warp ra (door.toCoords (areaDims newArea) new.coords) ro
    _ -> Ref.write new ro

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
    _ -> o { currentAnimation = Just a, currentFrame = 0, timeCounter = 0.0 }

deanimate :: String -> Ref Object -> Effect Unit
deanimate a = Ref.modify_ \o ->
  case o.currentAnimation of
    Just a' | a == a' -> o { currentAnimation = Nothing, currentFrame = 0, timeCounter = 0.0 }
    _ -> o

toggleAnimation :: String -> Ref Object -> Effect Unit
toggleAnimation a = Ref.modify_ \o ->
  case o.currentAnimation of
    Just a' | a == a' -> o { currentAnimation = Nothing, currentFrame = 0, timeCounter = 0.0 }
    Just _ -> o
    Nothing -> o { currentAnimation = Just a, currentFrame = 0, timeCounter = 0.0 }
