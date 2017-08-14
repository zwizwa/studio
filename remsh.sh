#!/bin/bash
cd $(dirname $0)
exec erl -remsh studio@zoe.zoo -name studio@core.zoo
