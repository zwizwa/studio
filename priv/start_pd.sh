#!/bin/bash
exec 2>&1
# socat sends a TERM signal when stdin closes
exec socat - EXEC:pd.local
