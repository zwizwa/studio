#!/bin/sh
exec 2>&1
# socat sends a TERM signal when stdin closes
exec socat -d -d -d - EXEC:pd.local
