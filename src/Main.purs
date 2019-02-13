module Main where
  
import Component (Component)
import DOM.HTML.DOM (api)
import DOM.VirtualDOM (VNode(..), createElement')
import Effect (Effect)
import Prelude (Unit, bind, pure, unit)
data App model event =  App (Component model event)

main :: Effect Unit
main = do 
    _ <- createElement' api  (Text "Aravindh")
    pure unit