#!/bin/bash
exec erl -remsh studio@zoe.zoo -name studio@core.zoo -setcookie `cat $(dirname $0)/.cookie`
