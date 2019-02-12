module DOM.VirtualDOM where
 
import Data.Show

import Data.Foldable (sequence_)
import Data.List ((!!), length, (..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Console (log)
import Partial.Unsafe (unsafePartial)
import Prelude (Unit, bind, pure, unit, void, ($), (>>=), (>>>), (>=>), (<>), (/=), map, (<#>), flip, when, (<<<), (>), (-), (<$>))
import Unsafe.Coerce (unsafeCoerce)
type Attribute = Tuple String String

data EventListener eventData = On String (eventData → Effect Unit)

type Props = Map.Map String String

data VNode v
  = Element
    { name :: String
    , props :: Props
    , listeners :: Array (EventListener v)
    , children :: Array (VNode v)
    }
  | Text String

instance showVNode :: Show (VNode v) where
  show (Element n) = "<VNode:" <> n.name <> ">"
  show (Text t) = "\"" <> t <> "\"" 

type VDOM v e l =
  { createElement :: String → Effect e
  , createElementNS :: String → String → Effect e
  , createTextNode :: String → Effect e 
  , replaceChild :: l → l → l → Effect Unit
  , removeChild :: l → l → Effect Unit
  , appendChild :: l → l → Effect Unit
  , childCount :: l → Effect Int
  , childAt :: Int → l → Effect (Maybe l)
  , setTextContent :: String → l → Effect Unit
  , setAttribute :: String → String → l → Effect Unit
  , removeAttribute :: String → l → Effect Unit
  , addEventListener :: String → (v → Effect Unit) → l → Effect Unit
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

createElement :: ∀ e l v. VDOM e l v → VNode e l v → Effect e
createElement api (Element e) = do
  el ← api.createElement e.name
  _ <- pure (map (\_ k v → api.setAttribute k v el) unit e.props)
  -- sequence_ $ e.listeners <#> addListener api el
  _ <- sequence_ $ e.children <#> (createElement api >=> flip api.appendChild el)
  pure el
createElement api (Text t) = api.createTextNode t

addListener :: ∀  v. VDOM  v → l → EventListener  v → Effect e Unit
addListener api target (On name handler) = api.addEventListener name handler target

changed :: ∀  v. VNode  v → VNode  v → Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: ∀  v e l. VDOM  v e l → l → Props → Props → Effect Unit
updateProps api target old new =
  sequence_ (update <$> Map.keys (Map.union old new))
  where
    update key =
      case Map.lookup key old, Map.lookup key new of
        Nothing, Just value → api.setAttribute key value target
        Just _, Nothing → api.removeAttribute key target
        Just prev, Just next → when (prev /= next) $ api.setAttribute key next target
        Nothing, Nothing → pure unit

patch :: ∀  v. VDOM  v → l → Maybe (VNode  v) → Maybe (VNode  v) → Effect e Unit
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
            _ <- case old, new of
              Element {props: oldProps}, Element {props: newProps} →
                updateProps api me oldProps newProps
              _, _ → pure unit
            walkChildren me old new

    walkChildren :: l → VNode  v → VNode  v → Effect e Unit
    walkChildren target (Element old) (Element new) = do
        if (oldLength > newLength)
          then do
            _ <- walkIndexes (0 .. (newLength - 1)) -- walk up to last child of new
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

