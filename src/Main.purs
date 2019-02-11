module Main where
  
import Effect (Effect)
import Prelude (Unit)
import HTML


data Model a = Model a

data Event a = Event a

type Reducer model event = Model model -> Event event -> Model model

data Component model event = Component { 
    init :: model
    , view :: model -> HTML
    , update:: model -> event -> model
    }

type HTML = String
type DOM = String

generateNode :: forall model event. App model event -> Node
generateNode app = Node { attributes: [], type:"div", children: []}

runApp :: forall model event. App model event -> model -> Effect  Unit
runApp app model = render (generateNode app)
    
data App model event =  App { 
        rootComponent :: Component model event
    }

main :: Effect Unit
main = do 
    let rootComponent = Component { init: 0 , view: (\x -> ""), update: (\model -> \e -> model)}
    let app =  App { rootComponent: rootComponent}
    runApp app 10