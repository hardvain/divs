module Main where
  
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, (>>>), (<<<), unit)
import Web.DOM.Document (Document, createElement, toNonElementParentNode)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (NonElementParentNode, getElementById)
import Web.HTML (HTMLDocument, HTMLElement, Window, window)
import Web.HTML.HTMLDocument (body, toDocument)
import Web.HTML.HTMLElement (fromElement, toNode)
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

render :: Node -> Effect Unit
render node = do
    currentWindow :: Window <- window :: Effect Window
    currentHTMLDocument <- document currentWindow :: Effect HTMLDocument
    let doc = toDocument currentHTMLDocument
    createdElement <- createElement "div" doc
    let nepn = toNonElementParentNode doc :: NonElementParentNode
    mayBeRoot <- (getElementById  "main" nepn) :: Effect (Maybe Element )
    let root =  (unsafePartial fromJust mayBeRoot) :: Element
    _ <- log "adding element"
    let rootNode = toNode (unsafePartial fromJust (fromElement root))
    _ <- appendChild (toNode (unsafePartial fromJust (fromElement createdElement))) rootNode
    pure unit


runApp :: forall model event. App model event -> model -> Effect  Unit
runApp app model = render (generateNode app)
    
data App model event =  App { 
        rootComponent :: Component model event
    }

main :: Effect Unit
main = do 
    let rootComponent = Component { init: 0 , view: (\x -> ""), update: (\model -> \e -> model)}
    let app =  App { rootComponent: rootComponent}
    runApp app 10