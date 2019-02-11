module Main where
  
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Prelude (Unit, bind, pure, (>>>), (<<<))
import Web.DOM.Document (Document, createElement)
import Web.DOM.Internal.Types (Element)
import Web.HTML (HTMLDocument, Window, window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.Window (document)
data Model a = Model a

data Event a = Event a

type Reducer model event = Model model -> Event event -> Model model

data Component model event = Component { 
    init :: model
    , view :: model -> HTML
    , update:: model -> event -> model
    }

type HTML = String
type Attribute = Tuple String String
type DOM = String
data Node = Node {
    attributes :: Array Attribute,
    type :: String,
    children :: Array Node
}

generateNode :: forall model event. App model event -> Node
generateNode app = Node { attributes: [], type:"div", children: []}

render :: Node -> Effect Element
render node = do
    currentWindow <- window :: Effect Window
    currentHTMLDocument <- document currentWindow :: Effect HTMLDocument
    let currentDocument = toDocument currentHTMLDocument :: Document
    createElement "div" currentDocument

runApp :: forall model event. App model event -> model -> Effect  Element
runApp app model = render (generateNode app)
    
data App model event =  App { 
        rootComponent :: Component model event
    }

main :: Effect Element
main = do 
    let rootComponent = Component { init: 0 , view: (\x -> ""), update: (\model -> \e -> model)}
    let app =  App { rootComponent: rootComponent}
    runApp app 10