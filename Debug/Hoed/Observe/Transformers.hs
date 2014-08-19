module Debug.Hoed.Observe.Transformers where

import qualified Control.Monad.Trans.State as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict
import Debug.Hoed.Observe

instance Monad m => Observable1 (Lazy.StateT s m) where
  observer1 comp p = do
    res <- comp
    send "<StateT>" (return return << res) p

instance Monad m => Observable1 (Strict.StateT s m) where
  observer1 comp p = do
    res <- comp
    send "<StateT>" (return return << res) p
