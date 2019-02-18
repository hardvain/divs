module Main where
  
import App (App, Component, Html)
import DOM.VirtualDOM (mount, text)
import DOM.Elements (code, div, h1, button, ul, li, h1_)
import Effect (Effect)
import Prelude (Unit)
import Button as B
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import DOM.Attributes
data Message = Succ | Pred 

fromButtonMessage :: B.Message -> Message
fromButtonMessage _ = Succ 


appRender :: Model -> Html Message
appRender {buttonModel} = div []  [ 
  div [style "color:red"] [text "children"] 
  , map (\_ -> Succ) (B.component.render buttonModel)
  , h1_ [text "Header"]
  , button [style ("color: red"), onClick (\_ -> Pred)] [text "pred"] 
  , button [style ("color: green"), onClick (\_ -> Succ)] [text "succ"]
  , code [] [text "value"]
  , ul [] [li []  [text "1"], li [] [text "2"], li [] [text "3"]]
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