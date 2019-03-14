#!/bin/bash

# Kill old instance, if any.  Is this still needed?
killall jackd
exec 2>&1
# socat sends jack a TERM signal when stdin closes
exec socat - EXEC:jackd.local

