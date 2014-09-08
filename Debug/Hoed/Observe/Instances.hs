module Debug.Hoed.Observe.Instances where

import Control.Monad.Trans.List

import qualified Control.Monad.Trans.State as Lazy
import qualified Control.Monad.Trans.State.Strict as Strict

import qualified Data.Map.Lazy as Lazy
import qualified Data.Map.Strict as Strict
import qualified Data.Set as Set

import Debug.Hoed.Observe

-- Containers
-- ----------

instance (Observable k) => Observable1 (Lazy.Map k) where
  observer1 x p = Lazy.fromDistinctAscList $ observer1 (Lazy.toList x) p

instance (Observable k, Observable a) => Observable (Lazy.Map k a) where
  observer = observer1
  observers = observers1

instance Observable1 Set.Set where
  observer1 x p = Set.fromDistinctAscList $ observer1 (Set.toList x) p

instance (Observable a) => Observable (Set.Set a) where
  observer = observer1
  observers = observers1

{-
instance (Observable k) => Observable1 (Strict.Map k) where
  observer1 x p = Strict.fromDistinctAscList $ observer1 (Strict.toList x) p

instance (Observable k, Observable a) => Observable (Strict.Map k a) where
  observer = observer1
  observers = observers1
-}

-- Monad Transformers
-- ------------------
observeComp name comp p = do
    res <- comp
    send name (return return << res) p

instance (Monad m) => Observable1 (ListT m) where
  observer1 = observeComp "<ListT>"

instance (Monad m, Observable a) => Observable (ListT m a) where
  observers = observers1
  observer = observer1
  
instance Monad m => Observable1 (Lazy.StateT s m) where
  observer1 = observeComp "<StateT>"

instance (Monad m, Observable a) => Observable(Lazy.StateT s m a) where
  observer = observer1
  observers = observers1

instance Monad m => Observable1 (Strict.StateT s m) where
  observer1 = observeComp "<StateT>"

instance (Monad m, Observable a) => Observable (Strict.StateT s m a) where
  observer = observer1
  observers = observers1
