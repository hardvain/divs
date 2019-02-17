module DOM.Channel where
  
import Data.Array
import Data.Maybe
import Prelude
import Data.Show
import Effect (Effect)
import Effect.Ref as Ref

data Channel a = Channel {
  current::  Ref.Ref (Maybe a),
  past:: Ref.Ref (Array a),
  handler :: a -> Effect Unit
}

send :: forall a. a -> Channel a -> Effect Unit
send a (Channel c) = do
  current <- Ref.read c.current
  case current of
    Just value -> do
      past <- Ref.read c.past
      Ref.write (value:past) c.past
      Ref.write (Just a) c.current
    Nothing -> do
      Ref.write (Just a) c.current
    
      
