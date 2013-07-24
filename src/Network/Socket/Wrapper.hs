{-# LANGUAGE CPP #-}
-- |
-- Module      : Network.Socket.Wrapper
-- Copyright   : 2013 Noriyuki OHKAWA
-- License     : BSD3
--
-- Maintainer  : n.ohkawa@gmail.com
-- Stability   : experimental
-- Portability : unknown
--
-- This module wrap Network.Socket deprecated IF.
module Network.Socket.Wrapper
    ( close
    , module Network.Socket
    ) where

import qualified Network.Socket as NS
#if MIN_VERSION_network(2,4,0)
import Network.Socket hiding ( close )
#else
import Network.Socket hiding ( sClose )
#endif

-- | wrap close/sClose
close :: Socket -> IO ()
#if MIN_VERSION_network(2,4,0)
close = NS.close
#else
close = NS.sClose
#endif
