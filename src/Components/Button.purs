module Button  where
  
import App (Html(..), Component(..))
import DOM.Elements (button)
import DOM.VirtualDOM (text)
import DOM.Combinators ((>->), (>=>), (>~>))
import Data.Tuple.Nested ((/\))
import Data.Maybe
import DOM.Attributes 
data Model = Type (Maybe String)

data Message = Unit

update :: Model -> Message -> Model
update model _ = model

render :: Model -> Html Message
render (Type (Just str)) = button [clazz str]  [text str] 
render _ = button [] [text "Click Me"] 

component :: Component Model Message
component = 
    { render: render
    , update: update
    , init: Type Nothing 
    }