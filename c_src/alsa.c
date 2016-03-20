#include "lib.h"
#include "port.h"
#include <alsa/asoundlib.h>

/* Midi input.
   Keep it simple.  Send raw midi on {port,1} protocol. */

// http://fundamental-code.com/midi/
// http://www.alsa-project.org/alsa-doc/alsa-lib/seq.html
// https://ccrma.stanford.edu/~craig/articles/linuxmidi/alsa-1.0/alsarawportlist.c

static snd_seq_t *seq_handle;
static int in_port;

void s_u8(uint8_t c) {putchar(c);}

#define SEND(...) do { \
        uint8_t data[] = {0, __VA_ARGS__}; \
        data[0] = sizeof(data) - 1;        \
        write(1, &data, sizeof(data));     \
    } while(0)

#define ASSERT_NN(cmd) do {int rv; if ((rv=(cmd))<0) { ERROR("%s -> %d\n", #cmd, rv); }} while(0)

int alsa_seq_in(int argc, char **argv) {
    const char *name = "exo";
    if (argc >= 2); name = argv[1];

    ASSERT_NN(snd_seq_open(&seq_handle, "default", SND_SEQ_OPEN_INPUT, 0));

    ASSERT_NN(snd_seq_set_client_name(seq_handle, name));
    ASSERT_NN(in_port = snd_seq_create_simple_port(
                  seq_handle, "input",
                  SND_SEQ_PORT_CAP_WRITE|SND_SEQ_PORT_CAP_SUBS_WRITE,
                  SND_SEQ_PORT_TYPE_APPLICATION));

    snd_seq_event_t *ev = NULL;
    uint32_t n = sizeof(*ev);
    uint8_t buf[1+n];
    buf[0] = n;
    while(1) {
        ASSERT_NN(snd_seq_event_input(seq_handle, &ev));
        memcpy(&buf[1], ev, n);
        assert_write(1, buf, n+1);
    }
    return 0;
}
int alsa_seq_out(int argc, char **argv) {
    return 0;
}
int alsa_midi_in(int argc, char **argv) {
    return 0;
}
int alsa_midi_out(int argc, char **argv) {
    return 0;
}
