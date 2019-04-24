// FIXME: untested skeleton

#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>
#include <semaphore.h>

sem_t sema;

int nb_in;  static jack_port_t **audio_in  = NULL;
static jack_client_t *client = NULL;

int write_buf = 0;
int read_buf  = 0;

#define CHUNK_FRAMES 4096

jack_default_audio_sample_t buf[2][CHUNK_FRAMES];

unsigned int frames = 0;

static int process (jack_nframes_t nframes, void *arg) {

    /* Copy incoming audio. */
    for (int in=0; in<nb_in; in++) {
        jack_default_audio_sample_t *src =
            jack_port_get_buffer(audio_in[in], nframes);
        jack_default_audio_sample_t *dst =
            &buf[write_buf][frames];

        ASSERT(frames + nframes <= CHUNK_FRAMES);
        memcpy(dst, src, sizeof(*src) * nframes);
    }
    frames += nframes;

    /* Swap buffers when ready */
    if (frames >= CHUNK_FRAMES) {
        write_buf ^= 1;
        frames = 0;
        sem_post(&sema);
    }

    return 0;
}

#define CMD_SYNC 1
struct {
    uint8_t len;
    uint8_t cmd;
    uint8_t data[255];
} cmd;

int jack_audio(int argc, char **argv) {
    ASSERT(argc == 3);
    const char *client_name = argv[1];
    ASSERT_ERRNO(sem_init(&sema, 0 /* share btwn threads*/, 0 /* initval */));
    nb_in = atoi(argv[2]);  audio_in  = calloc(nb_in,sizeof(void*));
    jack_status_t status;
    client = jack_client_open (client_name, JackNullOption, &status);
    char port_name[32] = {};
    for (int in = 0; in < nb_in; in++) {
        snprintf(port_name,sizeof(port_name)-1,"audio_in_%d",in);
        ASSERT(audio_in[in] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_AUDIO_TYPE, JackPortIsInput, 0));
    }

    jack_set_process_callback (client, process, 0);
    ASSERT(!mlockall(MCL_CURRENT | MCL_FUTURE));
    ASSERT(!jack_activate(client));

    for(;;) {
        cmd.len = assert_read_port8(0, &cmd.cmd);
        ASSERT(cmd.len > 0);
        switch(cmd.cmd) {
        case CMD_SYNC:
            ASSERT_ERRNO(sem_wait(&sema));
            assert_write(1,(void*)&buf[read_buf][0], sizeof(buf[read_buf]));
            read_buf ^= 1;
            break;
        default:
            LOG("unknown %d (%d)\n", cmd.cmd, cmd.len);
            exit(2);
        }
    }

    return 0;
}
