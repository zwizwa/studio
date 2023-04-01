// Playground for midi + audio synth.
// Midi part is cloned from jack_midi.c

#define _DEFAULT_SOURCE

#include "lib.h"
#include "sysex.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>
#include <pthread.h>
#include <semaphore.h>

static int nb_midi_in;  static jack_port_t **midi_in   = NULL;
static int nb_out;      static jack_port_t **audio_out = NULL;

static jack_client_t          *client = NULL;

static inline void process_midi(jack_nframes_t nframes) {
    for (int in=0; in<nb_midi_in; in++) {
        void *midi_in_buf  = jack_port_get_buffer(midi_in[in], nframes);
        jack_nframes_t n = jack_midi_get_event_count(midi_in_buf);
        for (jack_nframes_t i = 0; i < n; i++) {
            jack_midi_event_t event;
            jack_midi_event_get(&event, midi_in_buf, i);
            const uint8_t *msg = event.buffer;
            if (in == 0 &&
                event.size == 3 &&
                msg[0] == 0xB0 && // CC channel 0
                (msg[1] >= 23) && // CC num on Easycontrol 9
                (msg[1] <= 31)) {
                // ...
            }
        }
    }
}
static inline void process_audio(jack_nframes_t nframes) {
    for (int out=0; out<nb_out; out++) {
        jack_default_audio_sample_t *dst =
            jack_port_get_buffer(audio_out[out], nframes);
        for (int t=0; t<nframes; t++) {
            dst[t] = 0;
        }
    }
}
static int process (jack_nframes_t nframes, void *arg) {
    /* Order is important. */
    process_midi(nframes);
    process_audio(nframes);
    return 0;
}


int jack_synth(int argc, char **argv) {

    /* Jack client setup */
    const char *client_name = argv[1];
    nb_midi_in = 1;
    midi_in = calloc(nb_midi_in,sizeof(void*));
    jack_status_t status;
    client = jack_client_open (client_name, JackNullOption, &status);
    char port_name[32] = {};
    for (int in = 0; in < nb_midi_in; in++) {
        snprintf(port_name,sizeof(port_name)-1,"midi_in_%d",in);
        ASSERT(midi_in[in] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0));
    }
    for (int out = 0; out < nb_out; out++) {
        snprintf(port_name,sizeof(port_name)-1,"audio_out_%d",out);
        ASSERT(audio_out[out] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_AUDIO_TYPE, JackPortIsOutput, 0));
    }
    jack_set_process_callback (client, process, 0);
    ASSERT(!mlockall(MCL_CURRENT | MCL_FUTURE));
    ASSERT(!jack_activate(client));

    /* Input loop. */
    for(;;) {
        /* Not processing stdin. */
        sleep(1);
    }
    return 0;
}
