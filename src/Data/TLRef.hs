module Data.TLRef (TLRef, newTLRef, writeTLRef, readTLRef) where

import qualified Data.Map as M
import Data.IORef
import Control.Concurrent
import Control.Applicative

-- | A variable that stores at most one value per thread
newtype TLRef a = TLRef (IORef (M.Map ThreadId (IORef a)))

-- | Create a new thread local reference.
newTLRef :: IO (TLRef a)
newTLRef = TLRef <$> newIORef M.empty

writeTLRef :: TLRef a -> a -> IO ()
writeTLRef (TLRef mapRef) val = do
	tid <- myThreadId
	tlmap <- readIORef mapRef
	case M.lookup tid tlmap of
		Nothing -> do
			ref <- newIORef val
			atomicModifyIORef' mapRef (\tlmap' -> (M.insert tid ref tlmap', ()))
		Just ref -> writeIORef ref val

readTLRef :: TLRef a -> IO (Maybe a)
readTLRef (TLRef mapRef) = do
	mref <- M.lookup <$> myThreadId <*> readIORef mapRef
	case mref of
		Nothing -> return Nothing
		Just ref -> Just <$> readIORef ref
