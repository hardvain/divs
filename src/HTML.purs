module HTML where
 
import Component

import Data.Maybe (Maybe, fromJust)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, unit, (>>=), (>>>), ($), void, (>=>))
import Web.DOM (Text)
import Web.DOM.Document as Document

import Web.DOM.Element as Element
import Web.DOM.Internal.Types (Element)
import Web.DOM.Internal.Types (Node)
import Web.DOM.Node (appendChild)
import Web.DOM.NodeType (NodeType(..))
import Web.DOM.NonElementParentNode (NonElementParentNode, getElementById)
import Web.DOM.Text as Text
import Web.DOM.Node as Node
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (fromElement, toNode)
import Web.HTML.Window as Window
import Unsafe.Coerce (unsafeCoerce)

type Attribute = Tuple String String

data VNode = VNode {
    attributes :: Array Attribute,
    tpe :: String,
    children :: Array VNode
}

document :: Effect Document.Document
document = HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure

createElement :: String → Effect Node
createElement name = document >>= Document.createElement name >>= Element.toNode >>> pure

createTextNode ::  String → Effect Node
createTextNode t = document >>= Document.createTextNode t >>= Text.toNode >>> pure

replaceChild ::  Node → Node → Node → Effect Unit
replaceChild new old parent = void $ Node.replaceChild new old parent

removeChild ::  Node → Node → Effect Unit
removeChild child parent = void $ Node.removeChild child parent

appendChild ::  Node → Node → Effect Unit
appendChild child parent = void $ Node.appendChild child parent

setTextContent ::  String → Node → Effect Unit
setTextContent = Node.setTextContent

setAttribute :: String → String → Node → Effect Unit
setAttribute key value = unsafeCoerce >>> Element.setAttribute key value

removeAttribute :: String → Node → Effect Unit
removeAttribute key = unsafeCoerce >>> Element.removeAttribute key


nodeToDom :: String → VNode →  Effect Unit
nodeToDom rootId (VNode {attributes, tpe, children})  = do
  element <- createSpanElement "test" 
  setRootElement element rootId

setRootElement :: Node → String → Effect Unit
setRootElement node rootId = do
    currentWindow  <- HTML.window 
    doc <- document
    let nepn = Document.toNonElementParentNode doc :: NonElementParentNode
    mayBeRoot <- (getElementById  rootId nepn) :: Effect (Maybe Element )
    let root =  (unsafePartial fromJust mayBeRoot) :: Element
    _ <- log "adding element"
    let rootNode = toNode (unsafePartial fromJust (fromElement root))
    _ <- Node.appendChild node rootNode
    pure unit


createSpanElement :: String  → Effect Node
createSpanElement str  = createElement "span" 

createDivElement :: String  → Effect Node
createDivElement str  = createElement "div" 

createAnchorElement :: String  → Effect Node
createAnchorElement str  = createElement "a" 

createBoldElement :: String  → Effect Node
createBoldElement str  = createElement "bold" 

createButtonElement :: String  → Effect Node
createButtonElement str  = createElement "button" 

createLineBreakElement :: String  → Effect Node
createLineBreakElement str  = createElement "br" 