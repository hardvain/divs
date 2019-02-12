module Main where
  
import HTML

import Effect (Effect)
import Prelude (Unit, map,(>>>))


type Component model event =  { 
    init :: model
    , view :: model -> HTML
    , update:: model -> event -> model
}


data HTML = Text String
    | Span String
    | Div (Array HTML)
    | Anchor String
    | Bold String
    | Button HTML
    | LineBreak

htmlToNode :: HTML -> Node
htmlToNode (Text str) = Node { attributes: [], type:"", children: []}
htmlToNode (Span str) = Node { attributes: [], type:"span", children: [(htmlToNode (Text str))]}
htmlToNode (Div children) = Node { attributes: [], type:"div", children: map htmlToNode children}
htmlToNode (Anchor str) = Node { attributes: [], type:"a", children: [(htmlToNode (Text str))]}
htmlToNode (Bold str) = Node { attributes: [], type:"b", children: [(htmlToNode (Text str))]}
htmlToNode (Button child) = Node { attributes: [], type:"button", children: [htmlToNode child]}
htmlToNode (LineBreak) = Node { attributes: [], type:"br", children: []}

appToHtml :: forall model event. App model event -> HTML
appToHtml (App component) = component.view component.init

runApp :: forall model event. App model event -> model -> Effect  Unit
runApp app model = (appToHtml >>> htmlToNode >>> nodeToDom) app
    
data App model event =  App (Component model event)
main :: Effect Unit
main = do 
    let component = { init: 0 , view: (\x -> Text "test"), update: (\model -> \e -> model)}
    let app =  App component
    runApp app 10