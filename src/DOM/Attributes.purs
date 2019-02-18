module DOM.Attributes where
  
import Data.Tuple

import App (Attribute(..), EventListener(..))
import Web.Event.Internal.Types (Event)

clazz :: forall msg. String -> Attribute msg
clazz k = PropertyAttribute "class" k


style :: forall msg. String -> Attribute msg
style k = PropertyAttribute "style" k


id :: forall msg. String -> Attribute msg
id k = PropertyAttribute "id" k

onClick :: forall msg. (Event -> msg) -> Attribute msg 
onClick h = EventListenerAttribute "click" h

