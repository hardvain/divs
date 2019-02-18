module Main where
  
import App (App, Component, Html)
import DOM.VirtualDOM (mount, text)
import DOM.Events (onClick)
import DOM.Elements (code, div, h1, button, ul, li)
import Effect (Effect)
import Prelude (Unit)
import Data.Tuple.Nested ((/\))
import DOM.Combinators ((>->), (>=>), (>~>))
import Button as B
import Data.Functor (map)
import Data.Maybe (Maybe(..))
data Message = Succ | Pred 

fromButtonMessage :: B.Message -> Message
fromButtonMessage _ = Succ 


appRender :: Model -> Html Message
appRender {buttonModel} = div >->
  [ div >-> [text "children"] >=> ["style" /\ "color:red"]
  , map (\_ -> Succ) (B.component.render buttonModel)
  , h1  >-> [text ""] >=> ["style" /\ ("color: red")] 
  , button >-> [text "pred"] >=> ["style" /\ ("color: red")] >~> [onClick \_ -> Pred]
  , button >-> [text "succ"] >=> ["style" /\ ("color: green")] >~> [onClick \_ -> Succ]
  , code >-> [text "value"]
  , ul >-> [li >-> [text "1"], li >-> [text "2"], li >-> [text "3"]]
  ]

{-
  div [] $ do
    div [] $ do
      text "children"
      text $ show model
      button [id "sample", class "ant-button", ("data-key","value"), (onClick \_ -> Pred)] >>= text "pred"
      button [] >>= text "succ"
      code >>= text "value"
      ul $ do
        li (text "1")
        li (text "2")
        li (text "3")
-}
appUpdate :: Model -> Message -> Model
appUpdate model message = 
  case message of 
    Succ -> { buttonModel: B.Type (Just "succ")}
    Pred -> { buttonModel: B.Type (Just "pred")}

type Model =  
  { buttonModel :: B.Model
  } 


rootComponent :: Component Model Message
rootComponent = 
  { render : appRender
  , update : appUpdate
  , init : {buttonModel: B.Type (Just "Void")}
  }

app :: App Model Message
app = 
  { initialState: {buttonModel: B.Type (Just "Void")}
  , rootComponent : rootComponent
  }

main :: Effect Unit
main = mount "main" app