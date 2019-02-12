module Main where
  
import Component (Component, HTML(..))
import Data.Map (empty)
import Effect (Effect)
import DOM.VirtualDOM (VNode(..), nodeToDom)
import Prelude (Unit, map, (>>>))

htmlToNode :: forall v. HTML → VNode v
htmlToNode (TextTag str) = Element { props: empty, name:"", listeners:[],children: []}
htmlToNode (SpanTag str) = Element { props: empty, name:"span", listeners:[],children: [(htmlToNode (TextTag str))]}
htmlToNode (DivTag children) = Element { props: empty, name:"div", listeners:[],children: map htmlToNode children}
htmlToNode (AnchorTag str) = Element { props: empty, name:"a", listeners:[],children: [(htmlToNode (TextTag str))]}
htmlToNode (BoldTag str) = Element { props: empty, name:"b", listeners:[],children: [(htmlToNode (TextTag str))]}
htmlToNode (ButtonTag child) = Element { props: empty, name:"button", listeners:[],children: [htmlToNode child]}
htmlToNode (LineBreakTag) = Element { props: empty, name:"br", listeners:[],children: []}

appToHtml :: forall model event. App model event → HTML
appToHtml (App component) = component.view component.init

runApp :: forall model event. App model event → model → Effect  Unit
runApp app model = (appToHtml >>> htmlToNode >>> nodeToDom "main") app
    
data App model event =  App (Component model event)
main :: Effect Unit
main = do 
    let component = { init: 0 , view: (\x → TextTag "test"), update: (\model → \e → model)}
    let app =  App component
    runApp app 10