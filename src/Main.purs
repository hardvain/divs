module Main where
  
import Component (Component)
import DOM.HTML.DOM (api)
import DOM.VirtualDOM (EventListener(..), VNode, createElement, h, prop, text, with)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit, bind, pure, show, unit, ($), (<>))
import Data.Tuple.Nested ((/\))
import Web.Event.Internal.Types (Event)

data App model event =  App (Component model event)

render :: VNode Event
render  = h "div" (prop [])
  [ h "h1" (prop ["style" /\ ("color: rgb(" <> show 256 <> ",0,0)")]) [text $ "Number " ]
  , with (h "button" (prop []) [text "pred"]) [On "click" \_ -> pure unit]
  , with (h "button" (prop []) [text "succ"]) [On "click" \_ -> pure unit]
  ]


main :: Effect Unit
main = do 
    maybeNode <- api.getElementById "main"
    case maybeNode of
        Just node -> do
            createdElement <- createElement api  $ render
            _ <- api.appendChild  createdElement node
            pure unit
        Nothing -> pure unit