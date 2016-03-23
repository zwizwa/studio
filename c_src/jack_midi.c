#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>

int nb_out; static jack_port_t **midi_out = NULL;
int nb_in;  static jack_port_t **midi_in  = NULL;
static jack_client_t          *client = NULL;

#define BPM_TO_PERIOD(sr,bpm) ((sr*60)/(bpm*24))

typedef uint32_t mask_t;

jack_nframes_t clock_period = BPM_TO_PERIOD(48000, 120);
jack_nframes_t clock_time;
mask_t clock_mask = -1;
jack_nframes_t clock_sub_period = 6;
jack_nframes_t clock_sub_state = 0;


int frames = 0;
unsigned int read_buf = 0, write_buf = 0;

#define NB_CMD_BUFS 4
#define CMD_BUF_SIZE 4096

ssize_t cmd_size[NB_CMD_BUFS];
uint8_t cmd_buf[NB_CMD_BUFS][CMD_BUF_SIZE];
struct command {
    uint8_t size; // {packet,1} size = sizeof(struct header) + midi data size
    uint8_t type;
    union {
        struct {
            mask_t  mask;
            uint8_t midibytes[0];
        } midi;
        uint8_t u8[0];
        char c[0];
    } data;
} __attribute__((__packed__));
#define CMD_MIDI 0
#define CMD_CONNECT 1


// TODO:
// - proper sync between threads (lock-free queue)
// - set tempo


static inline void send_midi(void *out_buf, jack_nframes_t time,
                             const void *data_buf, size_t nb_bytes) {
    //LOG("%d %d %d\n", frames, time, (int)nb_bytes);
    void *buf = jack_midi_event_reserve(out_buf, time, nb_bytes);
    if (buf) memcpy(buf, data_buf, nb_bytes);
}


static int process (jack_nframes_t nframes, void *arg) {
    void *out_buf[nb_out];
    for (int out=0; out<nb_out; out++) {
        out_buf[out] = jack_port_get_buffer(midi_out[out], nframes);
        jack_midi_clear_buffer(out_buf[out]);
    }
    /* Jack requires us to sort the events, so send the async data
       first using time stamp 0. */
    while(read_buf != write_buf) {
        ssize_t cmd_offset = 0;
        while (cmd_offset < cmd_size[read_buf]) {
            struct command *cmd = (void*)&cmd_buf[read_buf][cmd_offset];
            if (CMD_MIDI == cmd->type) {
                size_t nb_bytes = cmd->size - (1 + 4);
                /* Send to selected outputs */
                for (int out=0; out<nb_out; out++) {
                    if (cmd->data.midi.mask & (1 << out)) {
                        send_midi(out_buf[out], 0/*time*/, cmd->data.midi.midibytes, nb_bytes);
                    }
                }
                /* Reset clock on start */
                for (int i=0; i<nb_bytes; i++) {
                    if (cmd->data.midi.midibytes[i] == 0xFA) clock_sub_state = 0;
                }
            }
            cmd_offset += cmd->size+1;
        }
        read_buf = (read_buf + 1) % NB_CMD_BUFS;
    }

    /* Send out the clock bytes at the designated time slots */
    while(clock_time < nframes) {
        /* Clock pulse fits in current frame. */
        for (int out=0; out<nb_out; out++) {
            if (clock_mask & (1 << out)) {
                const uint8_t clock[] = {0xF8};
                send_midi(out_buf[out], clock_time, clock, sizeof(clock));
            }
        }
        /* Send subsampled clock to Erlang. */
        if (clock_sub_state == 0) {
            const uint8_t msg[] = {2,0xFF,0xF8};
            assert_write(1, msg, sizeof(msg));
        }
        /* Advance clock */
        clock_time += clock_period;
        clock_sub_state = (clock_sub_state + 1) % clock_sub_period;
    }
    /* Account for this frame */
    clock_time -= nframes;

    /* Forward incoming midi. */
    for (int in=0; in<nb_in; in++) {
        void *in_buf  = jack_port_get_buffer(midi_in[in], nframes);
        jack_nframes_t n = jack_midi_get_event_count(in_buf);
        for (jack_nframes_t i = 0; i < n; i++) {
            jack_midi_event_t event;
            jack_midi_event_get(&event, in_buf, i);
            uint8_t msg[event.size+2];
            msg[0] = event.size+1;
            msg[1] = in;
            memcpy(msg+2, event.buffer, event.size);
            assert_write(1, msg, event.size+2);
        }
    }
    frames++;
    return 0;
}




int jack_midi(int argc, char **argv) {
    ASSERT(argc == 4);
    const char *client_name = argv[1];
    nb_in  = atoi(argv[2]);  midi_in  = calloc(nb_in,sizeof(void*));
    nb_out = atoi(argv[3]);  midi_out = calloc(nb_out,sizeof(void*));
    jack_status_t status;
    client = jack_client_open (client_name, JackNullOption, &status);
    char port_name[32] = {};
    for (int in = 0; in < nb_in; in++) {
        snprintf(port_name,sizeof(port_name)-1,"midi_in_%d",in);
        ASSERT(midi_in[in] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0));
    }
    for (int out = 0; out < nb_out; out++) {
        snprintf(port_name,sizeof(port_name)-1,"midi_out_%d",out);
        ASSERT(midi_out[out] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0));
    }


    jack_set_process_callback (client, process, 0);
    ASSERT(!mlockall(MCL_CURRENT | MCL_FUTURE));
    ASSERT(!jack_activate(client));
    for(;;) {
        if ((cmd_size[write_buf] =
             read(0,cmd_buf[write_buf],CMD_BUF_SIZE)) < sizeof(struct command)) exit(1);
        ssize_t cmd_offset = 0;
        while (cmd_offset < cmd_size[write_buf]) {
            struct command *cmd = (void*)&cmd_buf[write_buf][cmd_offset];
            switch(cmd->type) {
            case CMD_MIDI:
                // Handled in process()
                break;
            case CMD_CONNECT: {
                char *src = cmd->data.c;
                char *dst = src + strlen(src) + 1;
                LOG("connect %s %s\n", src, dst);
                jack_connect(client, src, dst);
                break;
            }
            default:
                break;
            }
            cmd_offset += cmd->size+1;
        }
        write_buf = (write_buf + 1) % NB_CMD_BUFS;
    }
    return 0;
}
