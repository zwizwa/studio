#!/bin/bash
# This only works when stdio is a terminal.
cd $(dirname $0)
exec erl -remsh studio@zoe.zoo -name studio@core.zoo
