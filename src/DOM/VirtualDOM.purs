module DOM.VirtualDOM (Html(..), Props, EventListener(..), h, with, prop, text, DomApi, createElement, mount) where
 
import Data.Show

import Data.Array ((!!), length, (..))
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Console (log)
import Prelude (Unit, bind, map, pure, unit, when, ($), (-), (/=), (<<<), (<>), (>),(+), show)
import Web.Event.Internal.Types (Event)
import Web.DOM.Internal.Types (Node)


type Attribute = Tuple String String

data EventListener  msg = On String (Event → msg)

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

type DomApi ef msg =
  { createElement :: String → Effect ef
  , createElementNS :: String → String → Effect ef
  , createTextNode :: String → Effect ef 
  , replaceChild :: ef → ef → ef → Effect Unit
  , removeChild :: ef → ef → Effect Unit
  , appendChild :: ef → ef → Effect Unit
  , childCount :: ef → Effect Int
  , childAt :: Int → ef → Effect (Maybe ef)
  , setTextContent :: String → ef → Effect Unit
  , setAttribute :: String → String → ef → Effect Unit
  , removeAttribute :: String → ef → Effect Unit
  , addEventListener :: String → (Event → Effect msg) → ef → Effect Unit
  , getElementById :: String -> Effect (Maybe ef)
  }

h :: ∀ msg. String → Props → Array (Html msg) → Html msg
h name props children = Element {name, props, children, listeners: []}

prop :: Array (Tuple String String) → Props
prop = Map.fromFoldable

with :: ∀ msg. Html msg → Array (EventListener msg) → Html msg
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n

text :: ∀ msg. String → Html msg
text = Text

mount :: ∀  msg. String → DomApi Node msg → Html msg → Effect Unit
mount nodeToMount api vnode = do 
    nodeState <- Ref.new (Just vnode)
    maybeNode <- api.getElementById "main"
    case maybeNode of
        Just node -> do
            createdElement <- createElement api vnode
            _ <- api.appendChild  createdElement node
            pure unit
        Nothing -> pure unit

createElement :: ∀ ef msg. DomApi ef msg → Html msg → Effect ef
createElement api (Element e) = do
  ref <- Ref.new 0
  el ← api.createElement e.name
  _ <- pure (Foldable.traverse_ (\_ k v → api.setAttribute k v el) e.props)
  _ <- Foldable.traverse_ (\listener -> addListener api el listener ref)  e.listeners
  _ <- Foldable.traverse_ (\child -> appendChild' api el child) e.children
  pure el
createElement api (Text t) = api.createTextNode t

appendChild' :: forall ef msg. DomApi ef msg -> ef -> Html msg -> Effect ef
appendChild' api parent child = do
  createdChild <- createElement api child
  _ <- api.appendChild createdChild parent
  pure createdChild

addListener :: ∀  ef msg. DomApi ef msg → ef → EventListener msg → Ref.Ref Int -> Effect Unit
addListener api target (On name handler) ref = do
  api.addEventListener name eventHandler target
    where
      eventHandler = \eventData -> do
        value <- Ref.read ref
        _ <- Ref.write (value + 1) ref
        newValue <- Ref.read ref
        _ <- log (show newValue)
        pure (handler eventData)

changed :: ∀  msg. Html msg → Html msg → Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: ∀  ef msg. DomApi ef msg → ef → Props → Props → Effect Unit
updateProps api target old new = do
  let unionArray = Set.toUnfoldable
  Foldable.traverse_ update (Map.keys (Map.union old new))
  where
    update :: String -> Effect Unit
    update key =
      case Map.lookup key old, Map.lookup key new of
        Nothing, Just value → api.setAttribute key value target
        Just _, Nothing → api.removeAttribute key target
        Just prev, Just next → when (prev /= next) $ api.setAttribute key next target
        Nothing, Nothing → pure unit

patch :: ∀  ef msg. DomApi ef msg → ef → Maybe (Html msg) → Maybe (Html msg) → Effect Unit
patch api target' old' new' = patchIndexed target' old' new' 0
  where
    patchIndexed :: ef → Maybe (Html  msg) → Maybe (Html  msg) → Int → Effect Unit
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
            _ <- case old, new of
              Element {props: oldProps}, Element {props: newProps} →
                updateProps api me oldProps newProps
              _, _ → pure unit
            walkChildren me old new

    walkChildren :: ef → Html msg → Html msg → Effect Unit
    walkChildren target (Element old) (Element new) = do
        if (oldLength > newLength)
          then do
            _ <- walkIndexes (0 .. (newLength - 1)) -- walk up to last child of new
            walkIndexes ((oldLength - 1) .. newLength) -- delete children backwards from end
          else do
            walkIndexes (0 .. (newLength - 1))
      where
        walkIndexes = Foldable.sequence_ <<< map (\i → patchIndexed target (old.children !! i) (new.children !! i) i)
        oldLength = length old.children
        newLength = length new.children
    walkChildren _ _ _ = pure unit

