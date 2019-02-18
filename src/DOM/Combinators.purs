module DOM.Combinators where
  
import DOM.VirtualDOM

import Data.Map as Map
import Data.Tuple (Tuple)
import Prelude (($))
import App
withProps :: forall msg. Html msg -> Array (Tuple String String) -> Html msg
withProps (Element e) props = Element $ e {props = Map.fromFoldable props}
withProps t _ = t

withListeners :: forall msg. Html msg -> Array (EventListener msg) -> Html msg
withListeners (Element n) listeners = Element $ n {listeners = listeners}
withListeners n _ = n


withChildren :: forall msg. Html msg -> Array (Html msg) -> Html msg
withChildren (Element n) children = Element $ n {children = children}
withChildren n _ = n

infix 1 withListeners as >~>
infix 2 withProps as >=>
infix 3 withChildren as >->