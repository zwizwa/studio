#!/bin/bash
cd $(dirname $0)
exec erl \
     -pa ebin `find deps -name 'ebin'` \
     -eval 'studio:start()'

