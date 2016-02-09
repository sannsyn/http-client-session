module HTTPClient.Session
(
  Session,
  run,
  withManager,
  requestLBSResponse,
  requestLBSBody,
  requestBSBody,
  -- * Reexports
  A.Manager(..),
  A.HttpException(..),
  A.Request(..),
)
where

import BasePrelude
import MTLPrelude
import Control.Monad.Trans.Either
import qualified Network.HTTP.Client as A
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString as C


-- |
-- A session on an HTTP manager.
newtype Session a =
  Session (ReaderT A.Manager (EitherT A.HttpException IO) a)
  deriving (Functor, Applicative, Monad, MonadIO, MonadError A.HttpException)

run :: Session a -> A.Manager -> IO (Either A.HttpException a)
run (Session reader) manager =
  runEitherT $ runReaderT reader manager

withManager :: (A.Manager -> IO (Either A.HttpException a)) -> Session a
withManager handler =
  Session $ ReaderT $ \manager -> EitherT $ handler manager

requestLBSResponse :: A.Request -> Session (A.Response B.ByteString)
requestLBSResponse request =
  withManager $ \manager -> try $ A.httpLbs request manager

requestLBSBody :: A.Request -> Session B.ByteString
requestLBSBody request =
  fmap A.responseBody $ requestLBSResponse request

requestBSBody :: A.Request -> Session C.ByteString
requestBSBody request =
  fmap B.toStrict $ requestLBSBody request
