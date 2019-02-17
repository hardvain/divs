module DOM.VirtualDOM (Html(..), Props, EventListener(..), h, with, prop, text, createElement, mount, App) where
 
import Effect.Console

import DOM.HTML.DOM (api)
import Data.Array ((!!), length, (..))
import Data.Foldable as Foldable
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Set as Set
import Data.Show (class Show)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Ref as Ref
import FRP.Event as Event
import Prelude (Unit, bind, map, pure, unit, when, ($), (-), (/=), (<<<), (<>), (>), show)
import Web.DOM.Internal.Types (Node)
import Web.Event.Internal.Types (Event)

type App model message =  
  { render :: model -> Html message
  , update :: model -> message -> model
  , init :: model
  }
type Attribute = Tuple String String

data EventListener  msg = On String (Event -> msg)

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

h :: forall msg
  . String 
  -> Props 
  -> Array (Html msg) 
  -> Html msg
h name props children = Element {name, props, children, listeners: []}

prop :: Array (Tuple String String) -> Props
prop = Map.fromFoldable

with :: forall msg
  . Html msg 
  -> Array (EventListener msg) 
  -> Html msg
with (Element n) listeners = Element $ n {listeners = listeners}
with n _ = n

text :: forall msg
  . String 
  -> Html msg
text = Text

mount :: forall model msg
  . (Show msg) => String 
  -> App model msg 
  -> Effect Unit
mount nodeToMount app = do
  maybeNode <- api.getElementById "main"
  _ <- Foldable.traverse_ (runApp app ) maybeNode
  pure unit

runApp :: forall model msg
  . (Show msg) => App model msg 
  -> Node 
  -> Effect Unit
runApp app nodeToMount = do 
  runAppOnMessage nodeToMount app Nothing

runAppOnMessage :: forall msg model
  . (Show msg) => Node 
  -> App model msg 
  -> Maybe msg 
  -> Effect Unit
runAppOnMessage nodeToMount app maybeMsg = do
  currentState <- Ref.new app.init
  oldModel <- Ref.read currentState
  let modelToRender = case (map (app.update oldModel) maybeMsg) of
                        Just model -> model
                        Nothing -> oldModel
  let htmlToRender = (app.render modelToRender)
  event <- Event.create :: Effect { event :: Event.Event msg, push :: msg -> Effect Unit }
  _ <- Event.subscribe event.event (\i -> callback i nodeToMount app currentState event.push)
  updateLoop nodeToMount htmlToRender event.push

callback :: forall msg model. Show msg => msg -> Node -> App model msg -> Ref.Ref model -> (msg -> Effect Unit) -> Effect Unit
callback newMsg nodeToMount app modelRef channel = do
  oldModel <- Ref.read modelRef
  let modelToRender = app.update oldModel newMsg
  let htmlToRender = (app.render modelToRender)
  updateLoop nodeToMount htmlToRender channel
  

updateLoop :: forall msg. Show msg => Node -> Html msg -> (msg -> Effect Unit) -> Effect Unit
updateLoop nodeToMount htmlToRender channel = do
  createdElement <- createElement htmlToRender channel
  api.appendChild createdElement nodeToMount

createElement 
  :: forall  msg
  .  (Show msg) => Html msg 
  -> (msg -> Effect Unit) 
  -> Effect Node
createElement (Element e) channel = do
  el ← api.createElement e.name
  _ <- pure (Foldable.traverse_ (\_ k v -> api.setAttribute k v el) e.props)
  _ <- Foldable.traverse_ (\listener -> addListener el listener channel)  e.listeners
  _ <- Foldable.traverse_ (\child -> appendChild' el child channel) e.children
  pure el
createElement (Text t) channel = api.createTextNode t

appendChild' :: forall  msg
  . (Show msg) => Node 
  -> Html msg 
  -> (msg -> Effect Unit)
  -> Effect Node
appendChild' parent child channel = do
  createdChild <- createElement child channel
  _ <- api.appendChild createdChild parent 
  pure createdChild

addListener :: forall msg
  . (Show msg) => Node 
  -> EventListener msg 
  -> (msg -> Effect Unit)
  -> Effect Unit
addListener target (On name handler) channel  = do
  api.addEventListener name eventHandler target
    where
      eventHandler = \eventData -> do
        let result = handler eventData
        channel result

changed :: forall msg
  . (Show msg) => Html msg 
  -> Html msg 
  -> Boolean
changed (Element e1) (Element e2) = e1.name /= e2.name
changed (Text t1) (Text t2) = t1 /= t2
changed _ _ = true

updateProps :: Node -> Props -> Props -> Effect Unit
updateProps target old new = do
  let unionArray = Set.toUnfoldable
  Foldable.traverse_ update (Map.keys (Map.union old new))
  where
    update :: String -> Effect Unit
    update key =
      case Map.lookup key old, Map.lookup key new of
        Nothing, Just value -> api.setAttribute key value target
        Just _, Nothing -> api.removeAttribute key target
        Just prev, Just next -> when (prev /= next) $ api.setAttribute key next target
        Nothing, Nothing -> pure unit

patch :: forall msg
  . (Show msg) => Node 
  -> Maybe (Html msg) 
  -> Maybe (Html msg) 
  -> (msg -> Effect Unit)
  -> Effect Unit
patch target' old' new' channel = patchIndexed target' old' new' 0
  where
    patchIndexed :: Node -> Maybe (Html  msg) -> Maybe (Html  msg) -> Int -> Effect Unit
    patchIndexed _ Nothing Nothing _ = pure unit
    patchIndexed parent Nothing (Just new) _ = do
      el ← createElement new channel
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
            n ← createElement new channel
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

