module Main where
  
import HTML
import Component (Component, HTML(..))
import Effect (Effect)
import Prelude (Unit, map,(>>>))


htmlToNode :: HTML -> VNode
htmlToNode (Text str) = VNode { attributes: [], tpe:"", children: []}
htmlToNode (Span str) = VNode { attributes: [], tpe:"span", children: [(htmlToNode (Text str))]}
htmlToNode (Div children) = VNode { attributes: [], tpe:"div", children: map htmlToNode children}
htmlToNode (Anchor str) = VNode { attributes: [], tpe:"a", children: [(htmlToNode (Text str))]}
htmlToNode (Bold str) = VNode { attributes: [], tpe:"b", children: [(htmlToNode (Text str))]}
htmlToNode (Button child) = VNode { attributes: [], tpe:"button", children: [htmlToNode child]}
htmlToNode (LineBreak) = VNode { attributes: [], tpe:"br", children: []}

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