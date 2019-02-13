module DOM.VirtualDOM (VNode(..), Props, EventListener(..)) where
 
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

data EventListener eventData = On String (eventData → Effect Unit)

type Props = Map.Map String String

data VNode ev
  = Element
    { name :: String
    , props :: Props
    , listeners :: Array (EventListener ev)
    , children :: Array (VNode ev)
    }
  | Text String

instance showVNode :: Show (VNode v) where
  show (Element n) = "<VNode:" <> n.name <> ">"
  show (Text t) = "\"" <> t <> "\"" 

type VDOM ev ef =
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
  , addEventListener :: String → (ev → Effect Unit) → ef → Effect Unit
  , getElementById :: String -> Effect (Maybe ef)
  }

h :: ∀ v. String → Props → Array (VNode v) → VNode v
h name props children = Element {name, props, children, listeners: []}

prop :: Array (Tuple String String) → Props
prop = Map.fromFoldable

with :: ∀ v. VNode  v → Array (EventListener  v) → VNode  v
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n

text :: ∀ v. String → VNode v
text = Text

createElement :: ∀ ev ef. VDOM ev ef → VNode ev → Effect ef
createElement api (Element e) = do
  el ← api.createElement e.name
  _ <- pure (map (\_ k v → api.setAttribute k v el) e.props)
  -- sequence_ $ e.listeners <#> addListener api el
  _ <- Foldable.traverse_ (\listener -> addListener api el listener)  e.listeners
  _ <- Foldable.traverse_ (\child -> appendChild' api el child) e.children
  -- _ <- Foldable.sequence_ $ e.children <#> (createElement api >=> flip api.appendChild el)
  pure el
createElement api (Text t) = api.createTextNode t

appendChild' :: forall ev ef. VDOM ev ef -> ef -> VNode ev -> Effect ef
appendChild' api parent child = do
  createdChild <- createElement api child
  _ <- api.appendChild parent createdChild
  pure createdChild

addListener :: ∀  ev ef. VDOM  ev ef → ef → EventListener  ev → Effect Unit
addListener api target (On name handler) = api.addEventListener name handler target

changed :: ∀  v. VNode  v → VNode  v → Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: ∀  ev ef. VDOM  ev ef → ef → Props → Props → Effect Unit
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

patch :: ∀  ev ef. VDOM  ev ef → ef → Maybe (VNode  ev) → Maybe (VNode  ev) → Effect Unit
patch api target' old' new' = patchIndexed target' old' new' 0
  where
    patchIndexed :: ef → Maybe (VNode  ev) → Maybe (VNode  ev) → Int → Effect Unit
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

    walkChildren :: ef → VNode  ev → VNode  ev → Effect Unit
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

