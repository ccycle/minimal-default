{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Data.Default.Class where

import GHC.Generics

class HasDefaultValue a where
    defaultValue :: a
    default defaultValue :: (Generic a, GHasDefaultValue (Rep a)) => a
    defaultValue = to gdefaultValue

class GHasDefaultValue f where
    gdefaultValue :: f a

instance (GHasDefaultValue a, GHasDefaultValue b) => GHasDefaultValue (a :*: b) where
    gdefaultValue = gdefaultValue :*: gdefaultValue
instance GHasDefaultValue U1 where
    gdefaultValue = U1
instance (HasDefaultValue a) => GHasDefaultValue (K1 i a) where
    gdefaultValue = K1 defaultValue
instance (GHasDefaultValue a) => GHasDefaultValue (M1 i c a) where
    gdefaultValue = M1 gdefaultValue

instance (HasDefaultValue a) => HasDefaultValue (Maybe a) where
    defaultValue = pure defaultValue
instance (HasDefaultValue a) => HasDefaultValue (Either e a) where
    defaultValue = pure defaultValue
instance (HasDefaultValue a) => HasDefaultValue (IO a) where
    defaultValue = pure defaultValue
