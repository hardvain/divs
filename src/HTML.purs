module HTML where
 
import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, unit)
import Web.DOM.Document (createElement, toNonElementParentNode)
import Web.DOM.Internal.Types (Element)
import Web.DOM.Node (appendChild)
import Web.DOM.NonElementParentNode (NonElementParentNode, getElementById)
import Web.HTML (HTMLDocument, Window, window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (fromElement, toNode)
import Web.HTML.Window (document)

type Attribute = Tuple String String

data Node = Node {
    attributes :: Array Attribute,
    type :: String,
    children :: Array Node
}


nodeToDom :: Node -> Effect Unit
nodeToDom node = do
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

