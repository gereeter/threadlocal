module Data.TLRef (TLRef, newTLRef, readTLRef, writeTLRef, modifyTLRef) where

import qualified Data.Map as M
import Data.IORef
import Control.Concurrent
import Control.Applicative

-- | A variable that stores at one value per thread
data TLRef a = TLRef a (IORef (M.Map ThreadId (IORef a)))

-- | Create a new thread local reference
newTLRef :: a -> IO (TLRef a)
newTLRef def = TLRef def <$> newIORef M.empty

getIORef :: TLRef a -> IO (IORef a)
getIORef (TLRef def mapRef) = do
    tid <- myThreadId
    tlmap <- readIORef mapRef
    case M.lookup tid tlmap of
        Nothing -> do
            ref <- newIORef def
            atomicModifyIORef' mapRef (\tlmap' -> (M.insert tid ref tlmap', ()))
            return ref
        Just ref -> return ref

-- | Write a value to the current thread's variable
writeTLRef :: TLRef a -> a -> IO ()
writeTLRef tlref val = do
    ioref <- getIORef tlref
    writeIORef ioref val

-- | Read the current thread's value in a variable
readTLRef :: TLRef a -> IO a
readTLRef tlref = do
    ioref <- getIORef tlref
    readIORef ioref

modifyTLRef :: TLRef a -> (a -> a) -> IO ()
modifyTLRef tlref f = do
    ioref <- getIORef tlref
    modifyIORef ioref f
