module Bird.Main where

import Bird.Event (frames, key, keydown, keyup, listen, music, sound)
import Bird.Object (Animations, Area, Dims, WorldMap, activateArea, animate, areaDims, areaFloor, at, buildObject, by, deanimate, faceLeft, faceRight, leftEdge, move, render, rightEdge)
import Bird.UI (hide)
import Control.Bind (bind, discard, pure, when, unless, (>>=))
import Control.Semigroupoid ((>>>))
import Data.Array (length, (!!))
import Data.DivisionRing (negate)
import Data.Eq ((/=), (==))
import Data.EuclideanRing (mod, (-), (/))
import Data.Foldable (fold)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.HeytingAlgebra (not, (&&), (||))
import Data.Int (quot, toNumber)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord (max, min, (<), (<=), (>), (>=))
import Data.Semiring ((*), (+))
import Data.Show (show)
import Data.Tuple (Tuple(..))
import Data.Unit (Unit, unit)
import Effect (Effect)
import Effect.Console (error, log, logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, TextAlign(..), beginPath, clearRect, closePath, drawImageFull, fillText, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, lineTo, moveTo, scale, setCanvasHeight, setCanvasWidth, setFillStyle, setFont, setLineWidth, setStrokeStyle, setTextAlign, setTransform, stroke, translate, tryLoadImage, withContext)

windowBounds :: CanvasElement -> Effect Dims
windowBounds canvas = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  pure {w: width, h: height}

gravity :: Number
gravity = 300.0

birdMoveSpeed :: Number
birdMoveSpeed = 400.0

birdFlySpeed :: Number
birdFlySpeed = -700.0

birdSheet :: Animations
birdSheet =
  { defaultFrames
    :
    [ {coords: at (256.0 * 0.0) 0.0, dims: walkingDims, length: Nothing}
    ]
  , modeFrames: Map.fromFoldable
    [ ( Tuple "walking"
        [ {coords: at (256.0 * 0.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 1.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 2.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 3.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 4.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 5.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 6.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 7.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 8.0) 0.0, dims: walkingDims, length: Just 50.0}
        , {coords: at (256.0 * 9.0) 0.0, dims: walkingDims, length: Just 50.0}
        ]
      )
    , ( Tuple "jumping"
        [ {coords: at (256.0 * 0.0) 256.0, dims: flyingDims, length: Nothing}
        ]
      )
    , ( Tuple "flying"
        [ {coords: at (512.0 * 0.0) 1024.0, dims: flyingDims, length: Just 75.0}
        , {coords: at (512.0 * 1.0) 1024.0, dims: flyingDims, length: Just 75.0}
        , {coords: at (512.0 * 2.0) 1024.0, dims: flyingDims, length: Just 75.0}
        , {coords: at (512.0 * 3.0) 1024.0, dims: flyingDims, length: Just 75.0}
        , {coords: at (512.0 * 4.0) 1024.0, dims: flyingDims, length: Just 75.0}
        , {coords: at (512.0 * 5.0) 1024.0, dims: flyingDims, length: Just 75.0}
        , {coords: at (512.0 * 6.0) 1024.0, dims: flyingDims, length: Just 75.0}
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

forest :: Area
forest =
  { name: "Tranquility"
  , background: "url(./backgrounds/forest.png)"
  , dims: dims
  , scaling: scaling
  , floorHeight: 70.0
  , terrain: []
  , doors:
    [ leftEdge dims scaling "pineforest"
    ]
  }
  where
    dims = by 1305.0 1107.0
    scaling = 1.5

pineforest :: Area
pineforest =
  { name: "Towards the Mountain"
  , background: "url(./backgrounds/pineforest.png)"
  , dims: dims
  , scaling: scaling
  , floorHeight: 70.0
  , terrain: []
  , doors:
    [ leftEdge dims scaling "swamp"
    , rightEdge dims scaling "forest"
    ]
  }
  where
    dims = by 1309.0 1113.0
    scaling = 1.5

snowforest :: Area
snowforest =
  { name: "Snow"
  , background: "url(./backgrounds/snowforest.png)"
  , dims: dims
  , scaling: scaling
  , floorHeight: 70.0
  , terrain: []
  , doors:
    [ rightEdge dims scaling "pineforest"
    ]
  }
  where
    dims = by 1181.0 1005.0
    scaling = 1.5

swamp :: Area
swamp =
  { name: "Bubble And Burble"
  , background: "url(./backgrounds/swamp.png)"
  , dims: dims
  , scaling: scaling
  , floorHeight: 70.0
  , terrain: []
  , doors:
    [ leftEdge dims scaling "swamptrees"
    , rightEdge dims scaling "pineforest"
    ]
  }
  where
    dims = by 1301.0 1114.0
    scaling = 1.5

swamptrees :: Area
swamptrees =
  { name: "From Still Water"
  , background: "url(./backgrounds/swamptrees.png)"
  , dims: dims
  , scaling: scaling
  , floorHeight: 70.0
  , terrain: []
  , doors:
    [ leftEdge dims scaling "fattree"
    , rightEdge dims scaling "swamp"
    ]
  }
  where
    dims = by 1301.0 1114.0
    scaling = 1.5

fattree :: Area
fattree =
  { name: "Speak!"
  , background: "url(./backgrounds/fattree.png)"
  , dims: dims
  , scaling: scaling
  , floorHeight: 90.0
  , terrain: []
  , doors:
    [ rightEdge dims scaling "swamptrees"
    ]
  }
  where
    dims = by 1301.0 1106.0
    scaling = 1.5

cliff :: Area
cliff =
  { name: "Precipice"
  , background: "url(./backgrounds/cliff.png)"
  , dims: dims
  , scaling: 1.5
  , floorHeight: 130.0
  , terrain: []
  , doors: []
  }
  where dims = by 1204.0 1110.0

cliffsidepath :: Area
cliffsidepath =
  { name: "Another Path"
  , background: "url(./backgrounds/cliffsidepath.png)"
  , dims: dims
  , scaling: 1.5
  , floorHeight: 130.0
  , terrain: []
  , doors: []
  }
  where dims = by 1249.0 1278.0

world :: WorldMap
world = Map.fromFoldable
  [ Tuple "forest" forest
  , Tuple "pineforest" pineforest
  , Tuple "snowforest" snowforest
  , Tuple "swamp" swamp
  , Tuple "swamptrees" swamptrees
  , Tuple "fattree" fattree
  , Tuple "cliff" cliff
  , Tuple "cliffsidepath" cliffsidepath
  ]

game :: Effect Unit
game = do
  getCanvasElementById "canvas" >>= \c -> case c of
    Nothing -> error "Failed to find canvas element!"
    Just canvas -> do
      ctx <- getContext2D canvas

      buildObject "bird.png" birdSheet (at 1000.0 1000.0) (by 128.0 128.0) true \archBird -> do
        bird <- Ref.new archBird
        isFlying <- Ref.new false
        isMovingLeft <- Ref.new false
        isMovingRight <- Ref.new false

        area <- Ref.new forest
        activateArea canvas forest

        music "music/song1.wav"
        hide "title"

        keydown "ArrowLeft" do
          Ref.write true isMovingLeft
        keyup "ArrowLeft" do
          Ref.write false isMovingLeft
        keydown "ArrowRight" do
          Ref.write true isMovingRight
        keyup "ArrowRight" do
          Ref.write false isMovingRight
        keydown " " do
          Ref.write true isFlying
        keyup " " do
          Ref.write false isFlying
        frames $ \delta -> withContext ctx do
          -- move player
          flying <- Ref.read isFlying
          movingLeft <- Ref.read isMovingLeft
          movingRight <- Ref.read isMovingRight
          let seconds = delta / 1000.0
          let dx = case Tuple movingLeft movingRight of
                Tuple true false -> -(seconds * birdMoveSpeed)
                Tuple false true -> seconds * birdMoveSpeed
                _ -> 0.0
          let dy = if flying then seconds * birdFlySpeed else 0.0
          move canvas world area (by dx dy) bird

          -- apply gravity
          b <- Ref.read bird
          move canvas world area (by 0.0 (seconds * gravity)) bird
          new <- Ref.read bird

          -- animate
          when movingLeft $ faceLeft bird
          when movingRight $ faceRight bird
          if (not flying && (movingLeft /= movingRight)) then animate "walking" bird else deanimate "walking" bird
          when (not flying && new.coords.y > b.coords.y) $ animate "falling" bird
          when (new.coords.y == b.coords.y) $ deanimate "falling" bird
          if flying then animate "flying" bird else deanimate "flying" bird

          -- clear canvas
          bounds <- windowBounds canvas
          clearRect ctx {x: 0.0, y: 0.0, width: bounds.w, height: bounds.h}
          render ctx delta bird

          -- render
          a <- Ref.read area
          setTransform ctx {m11: 1.0, m12: 0.0, m21: 0.0, m22: 1.0, m31: 0.0, m32: 0.0}
          setFont ctx "100px Arial"
          setTextAlign ctx AlignRight
          fillText ctx a.name ((areaDims a).w - 50.0) 100.0

main :: Effect Unit
main = do
  started <- Ref.new false
  listen "title" "click" do
    repeat <- Ref.read started
    Ref.write true started
    unless repeat game
