

CFLAGS="
-I$ERL_TOOLS/include
-I$UC_TOOLS
-Wall -DMAIN=main -DREAD=read -DWRITE=write"
# FIXME: do this per elf
LDFLAGS="-lasound -ljack -lpthread"

# C sources for static library
C_LIB="alsa.c jack_control.c jack_midi.c jack_audio.c"

# C sources for application binaries
C_APP="studio.c"
