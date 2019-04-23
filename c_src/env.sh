
# FIXME: how to specify dependencies?
CFLAGS_ERL_TOOLS="-I../../erl_tools/include/"

CFLAGS="$CFLAGS_ERL_TOOLS -I../include -Wall -DMAIN=main -DREAD=read -DWRITE=write"
# FIXME: do this per elf
LDFLAGS="-lasound -ljack"

# C sources for static library
C_LIB="alsa.c	jack_control.c	jack_midi.c"

# C sources for application binaries
C_APP="studio.c"
