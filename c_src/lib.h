#ifndef LIB_H
#define LIB_H

#define _POSIX_C_SOURCE 1

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "port.h"
#include "macros.h"

int jack_midi(int argc, char **argv);
int jack_audio(int argc, char **argv);
int jack_control(int argc, char **argv);
int alsa_seq_in(int argc, char **argv);
int alsa_seq_out(int argc, char **argv);
int alsa_midi_in(int argc, char **argv);
int alsa_midi_out(int argc, char **argv);




#endif
