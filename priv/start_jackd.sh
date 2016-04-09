#!/bin/bash

# Jackd is started as a port program to monitor its output messages,
# but is not listening on stdin, so won't die when beam dies.  Work
# around this by killing old instance on start.
killall jackd
exec 2>&1
exec jackd.local
