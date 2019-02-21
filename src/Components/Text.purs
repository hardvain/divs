module Text  where
  
import App (Html(..), Component(..))
import DOM.Elements (span)
import DOM.VirtualDOM (text)
import Data.Tuple.Nested ((/\))
import Data.Maybe
import DOM.Attributes 
import Prelude

type Model = {
    value :: Int
}

data Message = Succ | Pred

update :: Model -> Message -> Model
update model Succ = { value :  model.value + 1 }
update model Pred =  { value :  model.value + 1 }


render :: Model -> Html Message
render m = span [] [text (show m.value)] 

init :: Model
init = {value:0}