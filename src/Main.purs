module Main where
  
import Component (Component)
import DOM.HTML.DOM (api)
import DOM.VirtualDOM (EventListener(..), Html, createElement, h, prop, text, with, mount)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit, bind, pure, unit, ($), (>>=))
import Data.Tuple.Nested ((/\))
import Web.Event.Internal.Types (Event)
import Effect.Console
data App model event =  App (Component model event)

data Message = Succ | Pred

render :: Html Message
render  = h "div" (prop [])
  [ h "h1" (prop ["style" /\ ("color: red")]) [text $ "Number " ]
  , with (h "button" (prop []) [text "pred"]) [On "click" \_ -> Succ]
  , with (h "button" (prop []) [text "succ"]) [On "click" \_ -> Pred]
  ]


main :: Effect Unit
main = mount "main" api render