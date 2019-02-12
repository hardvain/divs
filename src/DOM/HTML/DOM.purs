module DOM.HTML.DOM where

import Effect (Effect)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple)
import Prelude (Unit, bind, pure, void, ($), (<>), (>=>), (>>=), (>>>))
import Data.Show
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Internal.Types (Node)
import Web.DOM.Node as Node
import Web.DOM.Text as Text
import Web.DOM.NodeList as NodeList
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window
import Data.Map (Map, fromFoldable)
import DOM.VirtualDOM

      
document :: Effect Document.Document
document = HTML.window >>= Window.document >>= HTMLDocument.toDocument >>> pure

createElement :: String → Effect Node
createElement name = document >>= Document.createElement name >>= Element.toNode >>> pure

createElementNS :: String → String → Effect Node
createElementNS ns name = document >>= Document.createElementNS  (Just ns) name >>= Element.toNode >>> pure

createTextNode ::  String → Effect Node
createTextNode t = document >>= Document.createTextNode t >>= Text.toNode >>> pure

replaceChild ::  Node → Node → Node → Effect Unit
replaceChild new old parent = void $ Node.replaceChild new old parent

removeChild ::  Node → Node → Effect Unit
removeChild child parent = void $ Node.removeChild child parent

appendChild ::  Node → Node → Effect Unit
appendChild child parent = void $ Node.appendChild child parent

childCount ::  Node → Effect Int
childCount = Node.childNodes >=> NodeList.length

childAt :: Int → Node → Effect (Maybe Node)
childAt index node = Node.childNodes node >>= NodeList.item index 

nextSibling ::  Node → Effect (Maybe Node)
nextSibling = Node.nextSibling 

setTextContent ::  String → Node → Effect Unit
setTextContent = Node.setTextContent

setAttribute :: String → String → Node → Effect Unit
setAttribute key value = unsafeCoerce >>> Element.setAttribute key value

removeAttribute :: String → Node → Effect Unit
removeAttribute key = unsafeCoerce >>> Element.removeAttribute key

addEventListener :: String → (Event → Effect Unit) → Node → Effect Unit
addEventListener name handler node = do
    eventListener <- EventTarget.eventListener handler
    EventTarget.addEventListener (Event.EventType name) eventListener false (unsafeCoerce node)
