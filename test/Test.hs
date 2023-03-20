{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Test where

import Data.Default.Class
import GHC.Generics

newtype MyFlag = MyFlag Bool deriving stock (Show, Generic)
instance HasDefaultValue MyFlag where
    defaultValue = MyFlag True

newtype MyInt = MyInt Int deriving stock (Show, Generic)
instance HasDefaultValue MyInt where
    defaultValue = MyInt 0

data TestData = TestData {a :: MyFlag, b :: MyInt, c :: Maybe MyInt}
    deriving stock (Show, Generic)
    deriving anyclass (HasDefaultValue)

unit_defaultValue :: IO ()
unit_defaultValue = print (defaultValue :: TestData)
