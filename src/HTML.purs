module HTML where
 
import Component

import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, unit)
import Web.DOM (Text)
import Web.DOM.Document (Document, createTextNode, createElement, toNonElementParentNode)
import Web.DOM.Element as Element
import Web.DOM.Internal.Types (Element)
import Web.DOM.Internal.Types (Node)
import Web.DOM.Node (appendChild)
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.NonElementParentNode (NonElementParentNode, getElementById)
import Web.DOM.Text as Text
import Web.HTML (HTMLDocument, Window, window)
import Web.HTML.HTMLDocument (toDocument)
import Web.HTML.HTMLElement (fromElement, toNode)
import Web.HTML.Window (document)
type Attribute = Tuple String String

data VNode = VNode {
    attributes :: Array Attribute,
    tpe :: String,
    children :: Array Node
}

getCurrentDocument :: Effect Document
getCurrentDocument = do
    currentWindow :: Window <- window :: Effect Window
    currentHTMLDocument <- document currentWindow :: Effect HTMLDocument
    pure (toDocument currentHTMLDocument)

nodeToDom :: VNode -> String -> Effect Unit
nodeToDom (VNode {attributes, tpe, children}) rootId = do
  
  doc <- getCurrentDocument
  element <- createSpanElement str doc
  setRootElement element rootId

setRootElement :: Element -> String -> Effect Unit
setRootElement element rootId = do
    currentWindow :: Window <- window :: Effect Window
    currentHTMLDocument <- document currentWindow :: Effect HTMLDocument
    let doc = toDocument currentHTMLDocument
    let nepn = toNonElementParentNode doc :: NonElementParentNode
    mayBeRoot <- (getElementById  rootId nepn) :: Effect (Maybe Element )
    let root =  (unsafePartial fromJust mayBeRoot) :: Element
    _ <- log "adding element"
    let rootNode = toNode (unsafePartial fromJust (fromElement root))
    _ <- appendChild (toNode (unsafePartial fromJust (fromElement element))) rootNode
    pure unit


createSpanElement :: String -> Document -> Effect Element
createSpanElement str doc = createElement "span" doc

createDivElement :: String -> Document -> Effect Element
createDivElement str doc = createElement "div" doc

createAnchorElement :: String -> Document -> Effect Element
createAnchorElement str doc = createElement "a" doc

createBoldElement :: String -> Document -> Effect Element
createBoldElement str doc = createElement "bold" doc

createButtonElement :: String -> Document -> Effect Element
createButtonElement str doc = createElement "button" doc

createLineBreakElement :: String -> Document -> Effect Element
createLineBreakElement str doc = createElement "br" doc