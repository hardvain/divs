module Main where
  
import DOM.VirtualDOM (App, Html, h, mount, prop, text, with)
import DOM.Events as Events
import DOM.Elements as Elements
import Effect (Effect)
import Prelude (Unit, show, ($), (+), (-))
import Data.Tuple.Nested ((/\))
import DOM.Combinators

data Message = Succ | Pred 

appRender :: Model -> Html Message
appRender model = h "div" (prop [])
  [ h "h1" (prop ["style" /\ ("color: red")]) [text $ show model]
  , with (h "button" (prop []) [text "pred"]) [Events.onClick \_ -> Pred]
  , with (h "button" (prop []) [text "succ"]) [Events.onClick \_ -> Succ]
  , Elements.code >$> [text "value"]
  , h "ul" (prop []) [h "li" (prop []) [text "1"], h "li" (prop []) [text "2"], h "li" (prop []) [text "3"]]
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