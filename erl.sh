#!/bin/bash
cd $(dirname $0)
export ERL_LIBS=$(readlink -f .)
exec erl \
     -name "studio@$HOSTNAME.zoo" \
     -pa ebin `find deps -name 'ebin'` \
     -eval 'application:ensure_all_started(studio)'


