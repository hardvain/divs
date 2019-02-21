module DOM.VirtualDOM (h, with, prop, text, createElement, mount) where
 

import App (AppState, Component, EventCallback, EventListener(..), Props, Html(..), App(..))
import DOM.HTML.DOM (api)
import Data.Array ((!!), length, (..))
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Show (class Show)
import Data.Tuple (Tuple, fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event as Event
import Prelude (Unit, bind, map, pure, unit, when, ($), (-), (/=), (<<<), (<>), (>), (>>=), flip)
import Web.DOM.Internal.Types (Node)
import Web.Event.Internal.Types (Event)

h :: forall msg
  . String 
  -> Props 
  -> Array (Html msg) 
  -> Html msg
h name props children = Element {name, props, children, listeners: []}

prop :: Array (Tuple String String) -> Props
prop = Map.fromFoldable

fromProps :: Props -> Array (Tuple String String)
fromProps = Map.toUnfoldable

with :: forall msg
  . Html msg 
  -> Array (EventListener msg) 
  -> Html msg
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n

text :: forall msg. String -> Html msg
text = Text

mount :: forall model msg. String -> App model msg -> Effect Unit
mount nodeToMount app =  api.getElementById nodeToMount >>= Foldable.traverse_ (runApp app)

runApp :: forall msg model. App model msg -> Node -> Effect Unit
runApp app nodeToMount = do
  initModel <- Ref.new app.rootComponent.init
  let  {render} = app.rootComponent
  let htmlToRender = (render app.rootComponent.init)
  initHtml <- Ref.new htmlToRender
  let appState = {model: initModel, html: initHtml} 
  event <- Event.create 
  _ <- Event.subscribe event.event $ onMessage nodeToMount app appState event.push
  patch nodeToMount Nothing (Just htmlToRender) event.push

onMessage 
  :: forall msg model
  .  Node 
  -> App model msg 
  -> AppState model msg 
  -> EventCallback msg
  -> msg
  -> Effect Unit
onMessage  nodeToMount app {model:modelRef,html:htmlRef} eventCallback newMsg = do
  oldModel <- Ref.read modelRef
  let  {render, update} = app.rootComponent
  let newModel = update oldModel newMsg
  _ <- Ref.write newModel modelRef
  oldHtml <- Ref.read htmlRef
  let newHtml = (render newModel)
  patch nodeToMount (Just oldHtml) (Just newHtml) eventCallback
  
createElement :: forall msg.  Html msg -> EventCallback msg -> Effect Node
createElement html@(Element e) callback = 
  api.createElement e.name 
  >>= setAttributes html 
  >>= addListener html callback 
  >>= appendChild html callback
createElement (Text t) callback = api.createTextNode t

setAttributes :: forall msg. Html msg -> Node -> Effect Node
setAttributes (Element e) element = do
  _ <- Foldable.for_ (fromProps e.props) (\t -> api.setAttribute (fst t) (snd t) element)
  pure element
setAttributes _ element = pure element

appendChild :: forall msg.  Html msg -> EventCallback msg -> Node -> Effect Node
appendChild  html@(Element e) callback target = do
  _ <- Foldable.for_ e.children attach 
  pure target
    where 
      attach :: Html msg -> Effect Unit
      attach child = createElement child callback >>= (flip api.appendChild) target
appendChild  (Text t) callback target = api.createTextNode t

addListener :: forall msg.  Html msg -> EventCallback msg -> Node -> Effect Node
addListener  (Element e) callback target = do
  _ <- Foldable.for_ e.listeners attach 
  pure target
    where
      attach :: EventListener msg -> Effect Unit
      attach (On name handler) = api.addEventListener name (callback <<< handler) target
addListener  (Text t) callback target = pure target


changed :: forall msg. Html msg -> Html msg -> Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: Node -> Props -> Props -> Effect Unit
updateProps target old new = do
  let unionArray = Set.toUnfoldable
  Foldable.for_ (Map.keys (Map.union old new)) update 
  where
    update :: String -> Effect Unit
    update key =
      case Map.lookup key old, Map.lookup key new of
        Nothing, Just value -> api.setAttribute key value target
        Just _, Nothing -> api.removeAttribute key target
        Just prev, Just next -> when (prev /= next) $ api.setAttribute key next target
        Nothing, Nothing -> pure unit

patch :: forall msg
  .  Node 
  -> Maybe (Html msg) 
  -> Maybe (Html msg) 
  -> EventCallback msg
  -> Effect Unit
patch target' old' new' callback = patchIndexed target' old' new' 0
  where
    patchIndexed :: Node -> Maybe (Html  msg) -> Maybe (Html  msg) -> Int -> Effect Unit
    patchIndexed _ Nothing Nothing _ = pure unit
    patchIndexed parent Nothing (Just new) _ = do
      el ← createElement new callback
      api.appendChild el parent

    patchIndexed parent (Just _) Nothing index = do
      child ← api.childAt index parent
      case child of
        Just n -> api.removeChild n parent
        Nothing -> pure unit

    patchIndexed parent (Just (Text old)) (Just (Text new)) index =
      when (old /= new) do
        me ← api.childAt index parent
        maybe (pure unit) (\t -> api.setTextContent new t) me

    patchIndexed parent (Just old) (Just new) index = do
      me' ← api.childAt index parent
      case me' of
        Nothing -> pure unit
        Just me ->
          if (changed old new) then do
            n ← createElement new callback
            api.replaceChild n me parent
          else do
            _ <- case old, new of
              Element {props: oldProps}, Element {props: newProps} ->
                updateProps  me oldProps newProps
              _, _ -> pure unit
            walkChildren me old new

    walkChildren :: Node -> Html msg -> Html msg -> Effect Unit
    walkChildren target (Element old) (Element new) = do
        if (oldLength > newLength)
          then do
            _ <- walkIndexes (0 .. (newLength - 1)) -- walk up to last child of new
            walkIndexes ((oldLength - 1) .. newLength) -- delete children backwards from end
          else do
            walkIndexes (0 .. (newLength - 1))
      where
        walkIndexes = Foldable.sequence_ <<< map (\i -> patchIndexed target (old.children !! i) (new.children !! i) i)
        oldLength = length old.children
        newLength = length new.children
    walkChildren _ _ _ = pure unit

