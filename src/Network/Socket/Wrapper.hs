{-# LANGUAGE CPP #-}
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

close :: Socket -> IO ()
#if MIN_VERSION_network(2,4,0)
close = NS.close
#else
close = NS.sClose
#endif
