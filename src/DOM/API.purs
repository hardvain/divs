module DOM.API (DomApi) where

import Data.Maybe (Maybe)
import Effect (Effect)
import Prelude (Unit)
import Web.Event.Internal.Types (Event)


type DomApi ef msg =
  { createElement :: String -> Effect ef
  , createElementNS :: String -> String -> Effect ef
  , createTextNode :: String -> Effect ef 
  , replaceChild :: ef -> ef -> ef -> Effect Unit
  , removeChild :: ef -> ef -> Effect Unit
  , appendChild :: ef -> ef -> Effect Unit
  , childCount :: ef -> Effect Int
  , childAt :: Int -> ef -> Effect (Maybe ef)
  , setTextContent :: String -> ef -> Effect Unit
  , setAttribute :: String -> String -> ef -> Effect Unit
  , removeAttribute :: String -> ef -> Effect Unit
  , addEventListener :: String -> (Event -> Effect msg) -> ef -> Effect Unit
  , getElementById :: String -> Effect (Maybe ef)
  }

