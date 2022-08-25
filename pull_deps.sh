#!/bin/sh

# Too much hassle to set up dependencies, so just copy files

cd $(dirname $0)

mkdir -p include

dep() {
echo $1
# ls $FROM/include
cat <<EOF >include/$(basename $1)
// DO NOT EDIT
// Original files are in:
// https://github.com/zwizwa/erl_tools
// https://github.com/zwizwa/uc_tools
//
$(cat $1)
EOF
}

dep ../erl_tools/include/bert.h
dep ../erl_tools/include/port.h
dep ../erl_tools/include/system.h
dep ../uc_tools/ns_dump.h
dep ../uc_tools/ns_queue.h
dep ../uc_tools/gensym.h
dep ../uc_tools/macros.h
dep ../uc_tools/uc_tools_config.h
dep ../uc_tools/os_linux.h

git add include/*.h
