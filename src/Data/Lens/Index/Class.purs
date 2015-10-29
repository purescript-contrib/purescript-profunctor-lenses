module Data.Lens.Index.Class
 ( IndexKey
 , IndexValue
 ) where

import Prelude

import Data.Identity (Identity())
import Data.Map as M
import Data.Maybe (Maybe())
import Data.Set as S
import Data.StrMap as SM

class IndexKey m k

class IndexValue m v

instance indexKeyFn :: IndexKey (a -> b) a
instance indexKeyArray :: IndexKey (Array a) Int
instance indexKeyIdentity :: IndexKey (Identity a) Unit
instance indexKeyMap :: IndexKey (M.Map k v) k
instance indexKeyMaybe :: IndexKey (Maybe a) Unit
instance indexKeySet :: IndexKey (S.Set k) k
instance indexKeyStrMap :: IndexKey (SM.StrMap v) String

instance indexValueFn :: IndexValue (a -> b) b
instance indexValueArray :: IndexValue (Array a) a
instance indexValueIdentity :: IndexValue (Identity a) a
instance indexValueMap :: IndexValue (M.Map k v) v
instance indexValueMaybe :: IndexValue (Maybe a) a
instance indexValueSet :: IndexValue (S.Set k) Unit
instance indexValueStrMap :: IndexValue (SM.StrMap v) v
