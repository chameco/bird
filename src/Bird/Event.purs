module Bird.Event where

import Data.Unit (Unit, unit)
import Effect (Effect)

foreign import _listen :: Unit -> String -> String -> Effect Unit -> Effect Unit
listen :: String -> String -> Effect Unit -> Effect Unit
listen = _listen unit

foreign import _keyup :: Unit -> String -> Effect Unit -> Effect Unit
keyup :: String -> Effect Unit -> Effect Unit
keyup = _keyup unit

foreign import _keydown :: Unit -> String -> Effect Unit -> Effect Unit
keydown :: String -> Effect Unit -> Effect Unit
keydown = _keydown unit

foreign import _key :: Unit -> String -> Effect Unit -> Effect Unit
key :: String -> Effect Unit -> Effect Unit
key = _key unit

foreign import _mousedown :: Unit -> String -> (Number -> Number -> Number -> Effect Unit) -> Effect Unit
mousedown :: String -> (Number -> Number -> Number -> Effect Unit) -> Effect Unit
mousedown = _mousedown unit

foreign import _frames :: Unit -> (Number -> Effect Unit) -> Effect Unit
frames :: (Number -> Effect Unit) -> Effect Unit
frames = _frames unit

foreign import _after :: Unit -> Number -> Effect Unit -> Effect Unit
after :: Number -> Effect Unit -> Effect Unit
after = _after unit

foreign import _resize :: Unit -> Effect Unit -> Effect Unit
resize :: Effect Unit -> Effect Unit
resize = _resize unit

foreign import _setCanvasBackground :: Unit -> String -> Number -> Number -> Effect Unit
setCanvasBackground :: String -> Number -> Number -> Effect Unit
setCanvasBackground = _setCanvasBackground unit

foreign import _music :: Unit -> String -> Effect Unit
music :: String -> Effect Unit
music = _music unit

foreign import _sound :: Unit -> String -> Effect Unit
sound :: String -> Effect Unit
sound = _sound unit
