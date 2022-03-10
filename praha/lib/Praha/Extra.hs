-- |
-- Module      :  Praha.Extra
-- Copyright   :  Jan Hamal Dvořák
-- License     :  MIT
--
-- Maintainer  :  mordae@anilinux.org
-- Stability   :  unstable
-- Portability :  non-portable (ghc)
--

module Praha.Extra
  ( whenM
  , unlessM
  , tshow
  , guarded
  , showTrace
  , KeyValue(..)
  )
where
  import Prelude

  import Debug.Trace
  import Control.Monad
  import Control.Applicative
  import Data.Text (Text, pack)


  -- | Run the second value if the first value returns 'True'
  whenM :: Monad m => m Bool -> m () -> m ()
  whenM boolM action = boolM >>= (`when` action)
  {-# INLINE whenM #-}


  -- | Run the second value if the first value returns 'False'
  unlessM :: Monad m => m Bool -> m () -> m ()
  unlessM boolM action = boolM >>= (`unless` action)
  {-# INLINE unlessM #-}


  -- | Similar to 'show', but with 'Text'.
  tshow :: (Show a) => a -> Text
  tshow = pack . show
  {-# INLINE tshow #-}


  -- |
  -- Similar to 'guard', but uses a predicate to determine if a value
  -- is acceptable and fails with 'empty' if not.
  --
  guarded :: (Alternative f) => (a -> Bool) -> a -> f a
  guarded p x = if p x then pure x else empty


  -- |
  -- Similar to 'trace', but with a name and implicit 'Show'.
  --
  showTrace :: (Show a) => String -> a -> a
  showTrace name val = trace ("TRACE: " <> name <> " = " <> show val) val


  -- |
  -- A key-value pair.
  --
  class KeyValue kv k v | kv -> k v where
    (.=) :: k -> v -> kv
    infixr 8 .=


  instance KeyValue (a, b) a b where
    k .= v = (k, v)
    {-# INLINE (.=) #-}


-- vim:set ft=haskell sw=2 ts=2 et:
