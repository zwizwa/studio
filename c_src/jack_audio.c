#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>
#include <semaphore.h>

sem_t sema;

static int nb_in;  static jack_port_t **audio_in  = NULL;
static jack_client_t *client = NULL;

static int write_buf = 0;
static int read_buf  = 0;

#define CHUNK_FRAMES_LOG 12
#define CHUNK_FRAMES (1 << CHUNK_FRAMES_LOG)


struct audio {
    uint8_t type;
    uint8_t stamp;
    uint8_t nb_channels;
    uint8_t nb_frames_log;
    jack_default_audio_sample_t chunk[CHUNK_FRAMES];
};


static struct audio audio_buf[2];

static unsigned int frames = 0;

static int process (jack_nframes_t nframes, void *arg) {

    ASSERT(frames + nframes <= CHUNK_FRAMES);

    /* Copy incoming audio. */
    for (int in=0; in<nb_in; in++) {
        jack_default_audio_sample_t *src =
            jack_port_get_buffer(audio_in[in], nframes);
        jack_default_audio_sample_t *dst =
            &audio_buf[write_buf].chunk[frames];

        memcpy(dst, src, sizeof(*src) * nframes);
    }
    frames += nframes;

    /* Swap buffers when ready */
    if (frames >= CHUNK_FRAMES) {
        jack_nframes_t f = jack_last_frame_time(client);
        audio_buf[write_buf].stamp = f / nframes;
        write_buf ^= 1;
        frames = 0;
        sem_post(&sema);
    }

    return 0;
}

#define CMD_SYNC 1

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


    for (int i=0; i<2; i++) {
        audio_buf[i].nb_channels = audio_in;
        audio_buf[i].nb_frames_log = 12;
        audio_buf[i].type = 0;
    }

    for(;;) {
        uint32_t n = assert_read_u32(0);
        ASSERT(n == 1);
        uint8_t cmd;
        assert_read(0, &cmd, 1);
        switch(cmd) {
        case CMD_SYNC:
            ASSERT_ERRNO(sem_wait(&sema));
            assert_write_port32(1, (void*)&audio_buf[read_buf], sizeof(audio_buf[read_buf]));
            read_buf ^= 1;
            break;
        default:
            LOG("unknown command %d\n", cmd);
            exit(2);
        }
    }

    return 0;
}
