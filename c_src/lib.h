#ifndef LIB_H
#define LIB_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#incllude "system.h"

#define CT_ASSERT(name, pred) \
    typedef char nct_assert_##name[(pred) ? 1 : -1]
#define CT_ASSERT_STRUCT_SIZE(name, size) \
    CT_ASSERT(struct_##name, sizeof(struct name) == size)
#define CT_ASSERT_UNION_SIZE(name, size) \
    CT_ASSERT(union_##name, sizeof(union name) == size)

#define ARRAY_SIZE(x) \
    (sizeof(x)/sizeof(x[0]))

int jack_midi(int argc, char **argv);
int jack_control(int argc, char **argv);
int alsa_seq_in(int argc, char **argv);
int alsa_seq_out(int argc, char **argv);
int alsa_midi_in(int argc, char **argv);
int alsa_midi_out(int argc, char **argv);

int assert_write(int fd, const uint8_t *buf, uint32_t nb);

#endif
