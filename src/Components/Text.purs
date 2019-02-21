module Text  where
  
import App (Html(..), Component(..))
import DOM.Elements (span)
import DOM.VirtualDOM (text)
import Data.Tuple.Nested ((/\))
import Data.Maybe
import DOM.Attributes 
import Prelude

data Model = Model {
    value :: Int
}

data Message = Succ | Pred

update :: Model -> Message -> Model
update (Model model) Succ = Model { value :  model.value + 1 }
update (Model model) Pred = Model { value :  model.value + 1 }


render :: Model -> Html Message
render (Model m) = span [] [text (show m.value)] 

component :: Component Model Message
component = 
    { render: render
    , update: update
    , init: Model { value: 0 }
    }