#ifndef LIB_H
#define LIB_H
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>

#define LOG(...)   fprintf(stderr,__VA_ARGS__)
#define ERROR(...) do{LOG(__VA_ARGS__);exit(1);}while(0)

#define CT_ASSERT(name, pred) \
    typedef char nct_assert_##name[(pred) ? 1 : -1]
#define CT_ASSERT_STRUCT_SIZE(name, size) \
    CT_ASSERT(struct_##name, sizeof(struct name) == size)
#define CT_ASSERT_UNION_SIZE(name, size) \
    CT_ASSERT(union_##name, sizeof(union name) == size)

#define ASSERT(assertion) ({ \
            if(!(assertion)) { \
                LOG("%s: %d: ASSERT FAIL: " #assertion "\n", __FILE__, __LINE__); \
                exit(1); \
            } })
#define ASSERT_EQ(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a != _b) { \
                LOG("ASSERT FAIL: " #a "(%d) == " #b "(%d)\n", _a, _b); \
                exit(1); \
            } })
#define ASSERT_NEQ(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a == _b) { \
                LOG("ASSERT FAIL: " #a "(%d) != " #b "(%d)\n", _a, _b); \
                exit(1); \
            } })
#define ASSERT_GT(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a <= _b) { \
                LOG("ASSERT FAIL: " #a "(%d) <= " #b "(%d)\n", _a, _b); \
                exit(1); \
            } })

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
