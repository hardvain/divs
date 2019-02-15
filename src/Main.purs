module Main where
  
import DOM.HTML.DOM (api)
import DOM.VirtualDOM (App, EventListener(..), Html, h, runApp, prop, text, with)
import Effect (Effect)
import Prelude (Unit, show, ($), (+), (-))
import Data.Tuple.Nested ((/\))

data Message = Succ | Pred

appRender :: Model -> Html Message
appRender model = h "div" (prop [])
  [ h "h1" (prop ["style" /\ ("color: red")]) [text $ show model ]
  , with (h "button" (prop []) [text "pred"]) [On "click" \_ -> Succ]
  , with (h "button" (prop []) [text "succ"]) [On "click" \_ -> Pred]
  ]

appUpdate :: Model -> Message -> Model
appUpdate model message = 
  case message of 
    Succ -> model - 1
    Pred -> model + 1

type Model = Int

app :: App Model Message
app = 
  { render : appRender
  , update : appUpdate
  , init : 0
  }

main :: Effect Unit
main = runApp "main" api app