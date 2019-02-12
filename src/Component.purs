module Component where
  

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
