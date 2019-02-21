module Button  where
  
import App (Html(..), Component(..))
import DOM.Elements (button)
import DOM.VirtualDOM (text)
import Data.Tuple.Nested ((/\))
import Data.Maybe
import DOM.Attributes 
import Prelude

type Model = {
    text :: String
}

type Message = Unit

update :: Model -> Message -> Model
update model _ = model

render :: Model -> Html Message
render m = button [] [text m.text] 

init :: Model
init =  { text: "Button"}