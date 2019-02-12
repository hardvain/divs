module DOM.DOM where
 
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, unit, void, ($), (>>=), (>>>),(>=>), (<>))
import Data.Show
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document as Document
import Web.DOM.Element as Element
import Web.DOM.Internal.Types (Element, Node)
import Web.DOM.Node as Node
import Web.DOM.NonElementParentNode (NonElementParentNode, getElementById)
import Web.DOM.Text as Text
import Web.DOM.NodeList as NodeList
import Web.Event.Event as Event
import Web.Event.EventTarget as EventTarget
import Web.Event.Internal.Types (Event)
import Web.HTML as HTML
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.HTMLElement (fromElement, toNode)
import Web.HTML.Window as Window
import Data.Map (Map, fromFoldable)


type DOM e l v =
  { createElement :: String → Effect e l
  , createElementNS :: String → String → Effect e l
  , createTextNode :: String → Effect e l
  , replaceChild :: l → l → l → Effect e Unit
  , removeChild :: l → l → Effect e Unit
  , appendChild :: l → l → Effect e Unit
  , childCount :: l → Effect e Int
  , childAt :: Int → l → Effect e (Maybe l)
  , setTextContent :: String → l → Effect e Unit
  , setAttribute :: String → String → l → Effect e Unit
  , removeAttribute :: String → l → Effect e Unit
  , addEventListener :: String → (v → Effect e Unit) → l → Effect e Unit
  }

createElement :: ∀  v. DOM  v → VNode  v → Effect 
createElement api (Element e) = do
  el ← api.createElement e.name
  Map.foldM (\_ k v → api.setAttribute k v el) unit e.props
  sequence_ $ e.listeners <#> addListener api el
  sequence_ $ e.children <#> (createElement api >=> flip api.appendChild el)
  pure el
createElement api (Text t) = api.createTextNode t

addListener :: ∀  v. DOM  v → l → EventListener  v → Effect e Unit
addListener api target (On name handler) = api.addEventListener name handler target

changed :: ∀  v. VNode  v → VNode  v → Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: ∀  v. DOM  v → l → Props → Props → Effect e Unit
updateProps api target old new =
  sequence_ (update <$> Map.keys (Map.union old new))
  where
    update key =
      case Map.lookup key old, Map.lookup key new of
        Nothing, Just value → api.setAttribute key value target
        Just _, Nothing → api.removeAttribute key target
        Just prev, Just next → when (prev /= next) $ api.setAttribute key next target
        Nothing, Nothing → pure unit

patch :: ∀  v. DOM  v → l → Maybe (VNode  v) → Maybe (VNode  v) → Effect e Unit
patch api target' old' new' = patchIndexed target' old' new' 0
  where
    patchIndexed :: l → Maybe (VNode  v) → Maybe (VNode  v) → Int → Effect e Unit
    patchIndexed _ Nothing Nothing _ = pure unit
    patchIndexed parent Nothing (Just new) _ = do
      el ← createElement api new
      api.appendChild el parent

    patchIndexed parent (Just _) Nothing index = do
      child ← api.childAt index parent
      case child of
        Just n → api.removeChild n parent
        Nothing → pure unit

    patchIndexed parent (Just (Text old)) (Just (Text new)) index =
      when (old /= new) do
        me ← api.childAt index parent
        maybe (pure unit) (\t → api.setTextContent new t) me

    patchIndexed parent (Just old) (Just new) index = do
      me' ← api.childAt index parent
      case me' of
        Nothing → pure unit
        Just me →
          if (changed old new) then do
            n ← createElement api new
            api.replaceChild n me parent
          else do
            case old, new of
              Element {props: oldProps}, Element {props: newProps} →
                updateProps api me oldProps newProps
              _, _ → pure unit
            walkChildren me old new

    walkChildren :: l → VNode  v → VNode  v → Effect e Unit
    walkChildren target (Element old) (Element new) = do
        if (oldLength > newLength)
          then do
            walkIndexes (0 .. (newLength - 1)) -- walk up to last child of new
            walkIndexes ((oldLength - 1) .. newLength) -- delete children backwards from end
          else do
            walkIndexes (0 .. (newLength - 1))
      where
        walkIndexes = sequence_ <<< map (\i → patchIndexed target (old.children !! i) (new.children !! i) i)
        oldLength = length old.children
        newLength = length new.children
    walkChildren _ _ _ = pure unit

nodeToDom :: forall v. String → VNode  v →  Effect Unit
nodeToDom rootId node  = pure unit

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