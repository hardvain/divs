module Component where
  
import Effect (Effect)
import Prelude (Unit)

type Component model event =  { 
    init :: model
    , view :: model → HTML
    , update:: model → event → model
}

data HTML = TextTag String
    | SpanTag String
    | DivTag (Array HTML)
    | AnchorTag String
    | BoldTag String
    | ButtonTag HTML
    | LineBreakTag

