{-# LANGUAGE RecordWildCards #-}
module Forces
    ( forces
    )
where

import Control.Concurrent

import Types


forces :: MVar Types.Input -> MVar Types.State -> IO ()
forces input state = do
    i <- readMVar input
    modifyMVar_ state pure
