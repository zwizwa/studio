#!/bin/bash

# if [ $HOSTNAME != zoe ]; then
#     echo REMOTE
#     exec $(dirname $0)/remsh.sh
# fi

cd $(dirname $0)
export ERL_LIBS=$(readlink -f .)
exec erl \
     -name "studio@$HOSTNAME.zoo" \
     -pa ebin `find deps -name 'ebin'` \
     -eval 'application:ensure_all_started(studio)'


