module App where

import Data.Map as Map
import Data.Show (class Show)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Ref as Ref
import Prelude (Unit, (<>))
import Web.Event.Internal.Types (Event)


type App model message = 
  { initialState :: model
  , rootComponent :: Component model message
  }

type Component model message =  
  { render :: model -> Html message
  , update :: model -> message -> model
  , init :: model
  }

type AppState model message = {
  model :: Ref.Ref model,
  html :: Ref.Ref (Html message)
}

type Attribute = Tuple String String

data EventListener  msg = On String (Event -> msg)
type EventCallback msg = msg -> Effect Unit
type Props = Map.Map String String

data Html msg
  = Element
    { name :: String
    , props :: Props
    , listeners :: Array (EventListener msg)
    , children :: Array (Html msg)
    }
  | Text String

instance showHtml :: Show (Html msg) where
  show (Element n) = "<Html:" <> n.name <> ">"
  show (Text t) = "\"" <> t <> "\"" 

