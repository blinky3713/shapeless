{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
    ( someFunc
    ) where

import Control.Lens (Iso', iso, view, review)
import Data.Text (Text, pack, unpack)
import qualified GHC.Generics as GHC
import Generics.SOP (SOP(..), I(..), NP(..), NS(..), Rep, Generic(..))
import Control.Lens.Wrapped (Wrapped(..))



someFunc :: IO ()
someFunc = putStrLn $ show baseUser

newtype UserId = UserId Integer deriving (Eq, Show)

instance Wrapped UserId where
  type Unwrapped UserId = Int
  _Wrapped' = iso (\(UserId uid) -> fromInteger uid) (UserId . fromIntegral)

newtype UserName = UserName Text deriving (Eq, Show)

instance Wrapped UserName where
  type Unwrapped UserName = String
  _Wrapped' = iso (\(UserName un) -> unpack un) (UserName . pack)

data FancyUser =
  FancyUser { fancyUserName :: UserName
            , fancyUserId :: UserId
            } deriving (Eq, Show, GHC.Generic)

instance Generic FancyUser

data BaseUser =
  BaseUser { baseUserName :: String
           , baseUserId :: Int
           } deriving (Eq, Show, GHC.Generic)

instance Generic BaseUser

class Simplified c s where
  simplify :: c -> s

instance Simplified (NP I '[]) (NP I '[]) where
  simplify = id

instance (Wrapped c, Unwrapped c ~ s, Simplified (NP I cs) (NP I ss)) => Simplified (NP I (c ': cs)) (NP I (s ': ss)) where
  simplify (I c :* cs) = I (view _Wrapped' c) :* simplify cs

instance Simplified (NP f cs) (NP f ss) => Simplified (SOP f '[cs]) (SOP f '[ss]) where
  simplify (SOP (Z rep)) = SOP (Z $ simplify rep)

genericSimplify :: (Generic c, Rep c ~ crep, Generic s, Rep s ~ srep, Simplified crep srep) => c -> s
genericSimplify = to . simplify . from

baseUser :: BaseUser
baseUser =
  let fancyUser = FancyUser (UserName "Ted") (UserId 1)
  in genericSimplify fancyUser
