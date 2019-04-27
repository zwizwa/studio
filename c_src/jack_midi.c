#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>

/* MIDI I/O

Note that the Erlang side is not supposed to handle MIDI for synth
control.  Erlang jitter is too high to be used for music, so MIDI is
considered "data plane", and all connectivity should be set up inside
of jack.

*/


static int nb_midi_out; static jack_port_t **midi_out = NULL;
static int nb_midi_in;  static jack_port_t **midi_in  = NULL;
static jack_client_t          *client = NULL;

#define BPM_TO_PERIOD(sr,bpm) ((sr*60)/(bpm*24))

typedef uint32_t mask_t;

static jack_nframes_t clock_period = BPM_TO_PERIOD(48000, 120);
static jack_nframes_t clock_time;
static mask_t clock_mask = 0;
static jack_nframes_t clock_sub_period = 6;
static jack_nframes_t clock_sub_state = 0;


static uint8_t blocks = 0;  // rolling time stamp
static volatile unsigned int read_buf = 0, write_buf = 0;

#define NB_CMD_BUFS 8
#define CMD_BUF_SIZE 8

/* Incoming is only MIDI.  Doesn't need to be tagged. */
struct command {
    uint32_t size; // {packet,4}
    mask_t  mask;
    uint8_t midibytes[0];
} __attribute__((__packed__));
static ssize_t cmd_size[NB_CMD_BUFS];
static uint8_t cmd_buf[NB_CMD_BUFS][CMD_BUF_SIZE];




// TODO:
// - proper sync between threads (lock-free queue)
// - set tempo


static inline void send_midi(void *out_buf, jack_nframes_t time,
                             const void *data_buf, size_t nb_bytes) {
    //LOG("%d %d %d\n", frames, time, (int)nb_bytes);
    void *buf = jack_midi_event_reserve(out_buf, time, nb_bytes);
    if (buf) memcpy(buf, data_buf, nb_bytes);
}


// Collect MIDI data so a single write() syscall can be used.
static uint8_t to_erl_buf[4096];
static size_t to_erl_buf_bytes = 0;

void to_erl(uint8_t *buf, int nb, uint8_t stamp) {
    size_t msg_size = 4 + 1 + 1 + nb;
    if (to_erl_buf_bytes + msg_size > sizeof(to_erl_buf)) {
        LOG("midi buffer overflow\n");
        return;
    }
    uint8_t *msg = &to_erl_buf[to_erl_buf_bytes];
    set_u32be(msg, msg_size - 4); // {packet,4}
    msg[4] = 0xFF;                // virtual port for clock data
    msg[5] = stamp;               // rolling time stamp  (FIXME: use clock_time?)
    memcpy(&msg[6], buf, nb);
    to_erl_buf_bytes += msg_size;
}

static int process (jack_nframes_t nframes, void *arg) {

    to_erl_buf_bytes = 0;


    jack_nframes_t f = jack_last_frame_time(client);
    uint8_t stamp = (f / nframes);

    void *midi_out_buf[nb_midi_out];
    for (int out=0; out<nb_midi_out; out++) {
        midi_out_buf[out] = jack_port_get_buffer(midi_out[out], nframes);
        jack_midi_clear_buffer(midi_out_buf[out]);
    }
    /* Jack requires us to sort the events, so send the async data
       first using time stamp 0. */
    while(read_buf != write_buf) {
        ssize_t cmd_offset = 0;
        while (cmd_offset < cmd_size[read_buf]) {
            struct command *cmd = (void*)&cmd_buf[read_buf][cmd_offset];
            size_t nb_bytes = cmd->size - sizeof(mask_t);
            /* Send to selected outputs */
            for (int out=0; out<nb_midi_out; out++) {
                if (cmd->mask & (1 << out)) {
                    send_midi(midi_out_buf[out], 0/*time*/, cmd->midibytes, nb_bytes);
                }
            }
            /* Reset clock on start */
            for (int i=0; i<nb_bytes; i++) {
                if (cmd->midibytes[i] == 0xFA) clock_sub_state = 0;
            }
            cmd_offset += cmd->size+1;
        }
        read_buf = (read_buf + 1) % NB_CMD_BUFS;
    }


    /* Send out the clock bytes at the designated time slots */
    while(clock_time < nframes) {
        /* Clock pulse fits in current frame. */
        for (int out=0; out<nb_midi_out; out++) {
            if (clock_mask & (1 << out)) {
                //LOG("F8 on %d\n", out);
                const uint8_t clock[] = {0xF8};
                send_midi(midi_out_buf[out], clock_time, clock, sizeof(clock));
            }
        }

        /* Send to Erlang for recording. */
        uint8_t tick = {0xF8};
        to_erl(&tick, sizeof(tick), stamp);

        /* Send subsampled clock to Erlang */
        // FIXME: Still necessary?
        //if (clock_sub_state == 0) {
        //    const uint8_t msg[] = {0,0,0,2,0xFF,0xF8};
        //    assert_write(1, msg, sizeof(msg));
        //}
        /* Advance clock */
        clock_time += clock_period;
        clock_sub_state = (clock_sub_state + 1) % clock_sub_period;
    }
    /* Account for this frame */
    clock_time -= nframes;

    /* Forward incoming midi.

       Note: I'm not exactly sure whether it is a good idea to perform
       the write() call from this thread, but it seems the difference
       between a single semaphore system call and a single write to an
       Erlang port pipe is not going to be big.  So revisit if it ever
       becomes a problem.

       As compared to a previous implementation, this will now buffer
       all midi meassages and perform only a single write() call.

       // FIXME: add time stamps?
    */


    for (int in=0; in<nb_midi_in; in++) {
        void *midi_in_buf  = jack_port_get_buffer(midi_in[in], nframes);
        jack_nframes_t n = jack_midi_get_event_count(midi_in_buf);
        for (jack_nframes_t i = 0; i < n; i++) {
            jack_midi_event_t event;
            jack_midi_event_get(&event, midi_in_buf, i);

            to_erl(event.buffer, event.size, stamp);
        }
    }

    if (to_erl_buf_bytes) {
        //LOG("buf_bytes = %d\n", (int)to_erl_buf_bytes);
        assert_write(1, to_erl_buf, to_erl_buf_bytes);
    }



    blocks++;
    return 0;
}



int jack_midi(int argc, char **argv) {
    ASSERT(argc == 5);
    const char *client_name = argv[1];
    nb_midi_in  = atoi(argv[2]);  midi_in  = calloc(nb_midi_in,sizeof(void*));
    nb_midi_out = atoi(argv[3]);  midi_out = calloc(nb_midi_out,sizeof(void*));
    clock_mask  = atoi(argv[4]);
    LOG("clock_mask = %d\n", clock_mask);

    jack_status_t status;
    client = jack_client_open (client_name, JackNullOption, &status);
    char port_name[32] = {};
    for (int in = 0; in < nb_midi_in; in++) {
        snprintf(port_name,sizeof(port_name)-1,"midi_in_%d",in);
        ASSERT(midi_in[in] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_MIDI_TYPE, JackPortIsInput, 0));
    }
    for (int out = 0; out < nb_midi_out; out++) {
        snprintf(port_name,sizeof(port_name)-1,"midi_out_%d",out);
        ASSERT(midi_out[out] = jack_port_register(
                   client, port_name,
                   JACK_DEFAULT_MIDI_TYPE, JackPortIsOutput, 0));
    }
    jack_set_process_callback (client, process, 0);
    ASSERT(!mlockall(MCL_CURRENT | MCL_FUTURE));
    ASSERT(!jack_activate(client));
    for(;;) {
        uint32_t n = assert_read_u32(0);
        ASSERT(n + 4 < sizeof(struct command));
        struct command *cmd = (void*)&cmd_buf[write_buf];
        cmd->size = n;
        cmd_size[write_buf] = n;
        assert_read(0, &cmd_buf[write_buf][4], n - 4);
        LOG("msg: n=%d\n", n);
        write_buf = (write_buf + 1) % NB_CMD_BUFS;  // FIXME: req: atomic write!
    }
    return 0;
}
