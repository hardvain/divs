module Main where
  
import App
import DOM.VirtualDOM (mount, text)
import DOM.Events (onClick)
import DOM.Elements (code, div, h1, button, ul, li)
import Effect (Effect)
import Prelude (Unit, show, ($), (+), (-))
import Data.Tuple.Nested ((/\))
import DOM.Combinators ((>->), (>=>), (>~>))

data Message = Succ | Pred 

appRender :: Model -> Html Message
appRender model = div >->
  [ div >-> [text "children"] >=> ["style" /\ "color:red"]
  , h1  >-> [text $ show model] >=> ["style" /\ ("color: red")] 
  , button >-> [text "pred"] >=> ["style" /\ ("color: red")] >~> [onClick \_ -> Pred]
  , button >-> [text "succ"] >=> ["style" /\ ("color: green")] >~> [onClick \_ -> Succ]
  , code >-> [text "value"]
  , ul >-> [li >-> [text "1"], li >-> [text "2"], li >-> [text "3"]]
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