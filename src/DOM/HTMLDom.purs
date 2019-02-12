module HTMLDOM where

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

type Attribute = Tuple String String

data EventListener  v = On String (v → Effect Unit)

type Props = Map String String

data VNode v
  = Element
    { name :: String
    , props :: Props
    , listeners :: Array (EventListener  v)
    , children :: Array (VNode  v)
    }
  | Text String

instance showVNode :: Show (VNode v) where
  show (Element n) = "<VNode:" <> n.name <> ">"
  show (Text t) = "\"" <> t <> "\""
      
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

h :: ∀ v. String → Props → Array (VNode v) → VNode v
h name props children = Element {name, props, children, listeners: []}

prop :: Array (Tuple String String) → Props
prop = fromFoldable

with :: ∀ v. VNode  v → Array (EventListener  v) → VNode  v
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n

text :: ∀ v. String → VNode v
text = Text