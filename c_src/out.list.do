# Create a list of targets

# Allow top level to override this
[ -z "$TARGETS" ] && TARGETS=host

# FIXME: Disable other targets for now.
TARGETS=host

# FIXME: Split the C applications up again
APPS="studio"

# The convention is to use one item per line.
for target in $TARGETS; do
    for app in $APPS; do 
        echo $app.$target.elf >>$3; 
    done
done
