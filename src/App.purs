module App where

import DOM.VirtualDOM
import Prelude
import Effect.Ref as Ref

import Effect (Effect)
type Html a = a

type App model message =  
    { render :: model -> Html message
    , update :: model -> message -> model
    , init :: model
    }

