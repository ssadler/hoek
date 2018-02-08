module Network.Komodo.Prelude
  ( module ALL
  , exceptToFail
  ) where

import Control.Monad as ALL (join, when)
import Control.Applicative as ALL
import Control.Monad.Trans.Except as ALL
import Control.Monad.Trans.Class as ALL

import Data.ByteString as ALL (ByteString)
import Data.ByteString.Lazy as ALL (toStrict)
import Data.Functor.Identity as ALL
import Data.Maybe as ALL (fromJust)
import Data.Monoid as ALL
import Data.Set as ALL (Set)
import Data.Text as ALL (unpack)

import Network.Komodo.Errors as ALL
--import Network.Komodo.Data.Utils as ALL


-- Calls fail on exception
exceptToFail :: Monad m => Except String a -> m a
exceptToFail = either fail return . runIdentity . runExceptT