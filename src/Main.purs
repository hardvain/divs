module Main where
  
import Component (Component)
import DOM.HTML.DOM (api)
import DOM.VirtualDOM (VNode(..), createElement)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Prelude (Unit, bind, pure, unit)

data App model event =  App (Component model event)

main :: Effect Unit
main = do 
    maybeNode <- api.getElementById "main"
    case maybeNode of
        Just node -> do
            createdElement <- createElement api  (Text "Aravindh")
            _ <- api.appendChild  createdElement node
            pure unit
        Nothing -> pure unit