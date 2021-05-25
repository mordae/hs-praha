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
  )
where
  import Prelude
  import Control.Monad (when, unless)
  import Data.Text (Text, pack)


  -- | Run the second value if the first value returns 'True'
  whenM :: Monad m => m Bool -> m () -> m ()
  whenM boolM action = boolM >>= (`when` action)
  {-# INLINE whenM #-}


  -- | Run the second value if the first value returns 'False'
  unlessM :: Monad m => m Bool -> m () -> m ()
  unlessM boolM action = boolM >>= (`unless` action)
  {-# INLINE unlessM #-}


  tshow :: (Show a) => a -> Text
  tshow = pack . show
  {-# INLINE tshow #-}


-- vim:set ft=haskell sw=2 ts=2 et:
