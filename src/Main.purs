module Main where
  
import DOM.VirtualDOM (App, EventListener(..), Html, h, mount, prop, text, with)
import Effect (Effect)
import Prelude (Unit, show, ($), (+), (-))
import Data.Tuple.Nested ((/\))
import Data.Show

data Message = Succ | Pred 
instance messageShow :: Show Message where
  show Succ = "succ"
  show Pred = "pred"

appRender :: Model -> Html Message
appRender model = h "div" (prop [])
  [ h "h1" (prop ["style" /\ ("color: red")]) [text $ show model ]
  , with (h "button" (prop []) [text "pred"]) [On "click" \_ -> Pred]
  , with (h "button" (prop []) [text "succ"]) [On "click" \_ -> Succ]
  ]

appUpdate :: Model -> Message -> Model
appUpdate model message = 
  case message of 
    Succ -> model + 1
    Pred -> model - 1

type Model = Int

app :: App Model Message
app = 
  { render : appRender
  , update : appUpdate
  , init : 0
  }

main :: Effect Unit
main = mount "main" app