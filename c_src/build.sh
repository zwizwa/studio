#!/bin/bash

# FIXME: set dep locations explicitly in do.erl
# FIXME: let the .do files use this build.sh file

assert_vars() {
    for var in $@; do
        # echo "checking $var=\"$(eval "echo \$$var")\"" >&2
        if [ -z "$(eval "echo \$$var")" ]; then
             echo "$var is undefined" >&2
             exit 1
        fi
    done
}

assert_vars TYPE

[ -z "$STUDIO"    ] && export STUDIO=$(dirname $0)/..
[ -z "$ERL_TOOLS" ] && export ERL_TOOLS=$STUDIO/../erl_tools
[ -z "$UC_TOOLS"  ] && export UC_TOOLS=$STUDIO/../uc_tools

# Turn them into absolute paths.
STUDIO=$(readlink -f $STUDIO)
UC_TOOLS=$(readlink -f $UC_TOOLS)
ERL_TOOLS=$(readlink -f $ERL_TOOLS)


# echo "STUDIO=$STUDIO" >&2
# echo "UC_TOOLS=$UC_TOOLS" >&2
# echo "ERL_TOOLS=$ERL_TOOLS" >&2

# [ ! -z "$ARCH" ] && . $STUDIO/c_src/env.$ARCH.sh
. $STUDIO/c_src/env.sh

[ -z "$GCC" ] && GCC=gcc

case "$TYPE" in
    o)
        assert_vars ARCH GCC
        $GCC \
            -I$STUDIO/c_src \
            $CFLAGS \
            $CFLAGS_EXTRA \
            -MD -MF $D \
            -DFIRMWARE=\"$FIRMWARE\" \
            -DBUILD=\"$VERSION\" \
            -o $O \
            -c $C
        ;;
    ld)
        # This is not a real file for linux builds.
        ;;
    a)
        assert_vars A O
        ar -r $A $O #2>/dev/null
        ;;
    elf)
        assert_vars LD ARCH MAP E A

        # The LD name is fake. Use linker's defaults.
        if [ $(basename "$LD") != dynamic.$ARCH.ld ]; then
            echo "Only dynamic linking: ARCH=$ARCH LD=$LD"
            exit 1
        fi
        set -x
        $GCC $LDFLAGS -Wl,-Map=$MAP -o $E $O $O_SYSTEM $A $LDLIBS $ELF_LDLIBS

        ;;
    *)
        echo "TYPE=$TYPE unknown" >&2
        exit 1
        ;;
esac

