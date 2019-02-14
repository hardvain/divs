module DOM.VirtualDOM (VNode(..), Props, EventListener(..), h, with, prop, text, VDOM, createElement) where
 
import Data.Show

import Data.Foldable as Foldable
import Data.Array ((!!), length, (..))
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Tuple (Tuple)
import Effect (Effect)
import Prelude (Unit, bind, map, pure, unit, when, ($), (-), (/=), (<<<), (<>), (>))
type Attribute = Tuple String String

data EventListener eventData msg = On String (eventData → Effect msg)

type Props = Map.Map String String

data VNode ev msg
  = Element
    { name :: String
    , props :: Props
    , listeners :: Array (EventListener ev msg)
    , children :: Array (VNode ev msg)
    }
  | Text String

instance showVNode :: Show (VNode v msg) where
  show (Element n) = "<VNode:" <> n.name <> ">"
  show (Text t) = "\"" <> t <> "\"" 

type VDOM ev ef msg=
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
  , addEventListener :: String → (ev → Effect msg) → ef → Effect Unit
  , getElementById :: String -> Effect (Maybe ef)
  }

h :: ∀ v msg. String → Props → Array (VNode v msg) → VNode v msg
h name props children = Element {name, props, children, listeners: []}

prop :: Array (Tuple String String) → Props
prop = Map.fromFoldable

with :: ∀ v msg. VNode v msg → Array (EventListener v msg) → VNode v msg
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n

text :: ∀ v msg. String → VNode v msg
text = Text

createElement :: ∀ ev ef msg. VDOM ev ef msg→ VNode ev msg → Effect ef
createElement api (Element e) = do
  el ← api.createElement e.name
  _ <- pure (Foldable.traverse_ (\_ k v → api.setAttribute k v el) e.props)
  _ <- Foldable.traverse_ (\listener -> addListener api el listener)  e.listeners
  _ <- Foldable.traverse_ (\child -> appendChild' api el child) e.children
  pure el
createElement api (Text t) = api.createTextNode t

appendChild' :: forall ev ef msg. VDOM ev ef msg -> ef -> VNode ev msg -> Effect ef
appendChild' api parent child = do
  createdChild <- createElement api child
  _ <- api.appendChild createdChild parent
  pure createdChild

addListener :: ∀  ev ef msg. VDOM ev ef msg → ef → EventListener ev msg → Effect Unit
addListener api target (On name handler) = api.addEventListener name handler target

changed :: ∀  v msg. VNode v msg → VNode v msg → Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: ∀  ev ef msg. VDOM ev ef msg → ef → Props → Props → Effect Unit
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

patch :: ∀  ev ef msg. VDOM ev ef msg → ef → Maybe (VNode ev msg) → Maybe (VNode ev msg) → Effect Unit
patch api target' old' new' = patchIndexed target' old' new' 0
  where
    patchIndexed :: ef → Maybe (VNode ev msg) → Maybe (VNode ev msg) → Int → Effect Unit
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

    walkChildren :: ef → VNode ev msg → VNode ev msg → Effect Unit
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

