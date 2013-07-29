graceful
========

Library to write graceful shutdown / upgrade service.

[![Build Status](https://travis-ci.org/notogawa/graceful.png?branch=master)](https://travis-ci.org/notogawa/graceful)

# Install

~~~ {.bash}
$ cabal update
$ cabal install graceful
~~~

# Controll Graceful Application

A graceful application can be controlled with signals.
The master process supports the following signals

* TERM/INT
    * fast shutdown
* QUIT
    * graceful shutdown
* HUP
    * restart workers
        1. starting new worker processes
        2. graceful shutdown old worker processes
* USR2
    * upgrading an executable file (starting new master & worker processes)

# Upgrading Executable on the Fly

1. Send USR2
2. Send QUIT to old process
