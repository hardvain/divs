module Main where
  
import DOM.Attributes

import App (App, Component, Html)
import Button as B
import DOM.Elements (code, div, h1, button, ul, li, h1_, code_, div_, h1_, button_, ul_, li_, h1_)
import DOM.VirtualDOM (mount, text)
import Data.Functor (map)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit, unit, ($))
import Text as T
import Unsafe.Coerce (unsafeCoerce)

data Message = Succ | Pred 
type Model =  
  { buttonModel :: B.Model, textModel :: T.Model
  } 

render :: Model -> Html Message
render {buttonModel, textModel} = div []  [ 
  div [style "color:red"] [text "children"] 
  , unsafeCoerce $ B.render buttonModel
  , unsafeCoerce $ T.render textModel
  , h1_ [text "Header"]
  , button [style ("color: red"), onClick (\_ -> Pred)] [text "pred"] 
  , button [style ("color: green"), onClick (\_ -> Succ)] [text "succ"]
  , code_ [text "value"]
  , ul_ [li_ [text "1"], li_ [text "2"], li_ [text "3"]]
  ]


update :: Model -> Message -> Model
update model message = 
  case message of 
    Succ -> model { textModel= T.update model.textModel T.Succ}
    Pred -> model { textModel= T.update model.textModel T.Pred}


init :: Model
init = {buttonModel: B.init, textModel: T.init}

rootComponent :: Component Model Message
rootComponent = 
  { render : render
  , update : update
  , init : init
  }

app :: App Model Message
app = 
  { rootComponent : rootComponent
  }

main :: Effect Unit
main = mount "main" app