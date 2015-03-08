module Sliding.Cached(Cached(), newCached, newCachedM, getCached) where

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Data.Maybe

newtype Cached eff a = Cached
  { value  :: RefVal (Maybe a)
  , action :: Eff eff a
  }

foreign import unsafePerformEff """
function unsafePerformEff(m){
  return m();
}""" :: forall eff a. Eff eff a -> a

newCached :: forall eff v. v -> Cached eff v
newCached a = unsafePerformEff $ do
  v <- newRef (Just a)
  return $ Cached
    { value: v
    , action: return a
    }

newCachedM :: forall eff v. Eff eff v -> Cached eff v
newCachedM m = unsafePerformEff $ do
  v <- newRef Nothing
  return $ Cached
    { value: v
    , action: m
    }

getCached :: forall eff v. Cached (ref :: Ref | eff) v -> Eff (ref :: Ref | eff) v
getCached (Cached c) = readRef c.value >>= \v -> case v of
  Just v  -> return v
  Nothing -> do
    v <- c.action
    writeRef c.value (Just v)
    return v
