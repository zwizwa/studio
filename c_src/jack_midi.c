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

#define NB_CMD_BUFS 8
#define CMD_BUF_SIZE 8

/* Erlang incoming is only MIDI.

   Data is received in blocking mode in the low-priority thread, to be
   picked up by the process thread. */
struct command {
    uint32_t size;  // {packet,4}
    mask_t   mask;  // Allow broadcasting
    uint8_t  midibytes[0];
} __attribute__((__packed__));
static ssize_t cmd_size[NB_CMD_BUFS];
static uint8_t cmd_buf[NB_CMD_BUFS][CMD_BUF_SIZE];

static volatile unsigned int read_buf = 0, write_buf = 0;



/* Erlang outgoing is also only MIDI.

   Since this is called from the process thread, data is collected to
   spend only a single write() syscall. */

static uint8_t to_erl_buf[4096];
static size_t to_erl_buf_bytes = 0;

void to_erl(uint8_t *buf, int nb, uint8_t port, uint8_t stamp) {
    size_t msg_size = 4 + 1 + 1 + nb;
    if (to_erl_buf_bytes + msg_size > sizeof(to_erl_buf)) {
        LOG("midi buffer overflow\n");
        return;
    }
    uint8_t *msg = &to_erl_buf[to_erl_buf_bytes];
    set_u32be(msg, msg_size - 4); // {packet,4}
    msg[4] = port;                // MIDI port number
    msg[5] = stamp;               // rolling time stamp  (FIXME: use clock_time?)
    memcpy(&msg[6], buf, nb);
    to_erl_buf_bytes += msg_size;
}


/* Sequencer.

   The sequencer is based on a collection of rolling queues, one for
   each cycle length.  Fixed size items and power of 2 buffer sizes
   are used to simplify code.
*/
struct event {
    uint16_t phase;
    uint8_t nb_bytes;
    uint8_t port;
    uint8_t midi[4];
};
#define NB_EVENTS_LOG 8
#define NB_EVENTS (1<<NB_EVENTS_LOG)
#define NB_EVENTS_MASK (NB_EVENTS-1)
struct queue {
    struct event buf[NB_EVENTS];
    uint32_t read;
    uint32_t write;
};

static inline const struct event *queue_peek(struct queue *q) {
    if (q->read == q->write) {
        return NULL;
    }
    else {
        // FIXME: this needs to always return something useful.  needs a sentinel
        return &q->buf[q->read & NB_EVENTS_MASK];
    }
}
static inline void queue_read(struct queue *q, struct event *e) {
    *e = q->buf[q->read++ & NB_EVENTS_MASK];
}
static inline void queue_write(struct queue *q, const struct event *e) {
    // FIXME: full detect + maybe resize?
    q->buf[q->write++ & NB_EVENTS_MASK] = *e;
}

/* TODO: The sequencer is under midi control.  Each loop has an input
 * mask for port events.  Recording is gated.  Start with 9 sequences
 * mapped to cc23-cc31, corresponding to the Easycontrol 9. */
struct track {
    struct queue q;
    uint32_t phase;
    uint32_t period;
    uint32_t port_mask;
};
static inline void track_tick(struct track *t) {
    if (!t->period) return; // FIXME
    t->phase = (t->phase + 1) % t->period;
}
static inline int track_next(struct track *t, struct event *e) {
    struct event *pe = queue_peek(&t->q);
    if (!pe || (t->phase != pe->phase)) {
        // no events (at all) or none at this phase. nothing to do.
        return 0;
    }
    else {
        // there is an event, so cycle the queue
        queue_read(&t->q, e);
        queue_write(&t->q, e);
        return 1;
    }
}
static inline void track_record(struct track *t, uint8_t port,
                                uint8_t *midi, int32_t nb_bytes) {
    struct event e = {
        .phase = t->phase,
        .nb_bytes = nb_bytes,
        .port = port
    };
    memcpy(&e.midi, &midi, nb_bytes);
    queue_write(&t->q, &e);
}

/* If record bit is set for a particular track, and the track's
 * port_mask is valid for the current port, record the event. */
uint32_t record_mask;
uint32_t playback_mask;

#define NB_TRACKS 9
struct track track[NB_TRACKS];


// TODO:
// - proper sync between threads (lock-free queue?)
// - set tempo


static inline void send_midi(void *out_buf, jack_nframes_t time,
                             const void *data_buf, size_t nb_bytes) {
    //LOG("%d %d %d\n", frames, time, (int)nb_bytes);
    void *buf = jack_midi_event_reserve(out_buf, time, nb_bytes);
    if (buf) memcpy(buf, data_buf, nb_bytes);
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
            cmd_offset += cmd->size+1;
        }
        read_buf = (read_buf + 1) % NB_CMD_BUFS;
    }


    /* Send out the MIDI clock bytes at the designated time slots */
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
        to_erl(&tick, sizeof(tick), 0xff, stamp);

        /* Advance clock */
        clock_time += clock_period;
    }
    /* Account for this frame */
    clock_time -= nframes;


    /* Play back sequences */
    for (int t=0; t<NB_TRACKS; t++) {
        if (playback_mask & (1 << t)) {
            struct event e;
            while (track_next(&track[t], &e)) {
                // Send the event out
            }
        }
        /* Advance time base once per Jack frame. */
        track_tick(&track[t]);
    }


    /* Forward incoming midi.

       Note: I'm not exactly sure whether it is a good idea to perform
       the write() call from this thread, but it seems the difference
       between a single semaphore system call and a single write to an
       Erlang port pipe accessing a single page of memory is not going
       to be big.  So revisit if it ever becomes a problem.

       As compared to a previous implementation, this will now buffer
       all midi meassages and perform only a single write() call.
    */


    for (int in=0; in<nb_midi_in; in++) {
        void *midi_in_buf  = jack_port_get_buffer(midi_in[in], nframes);
        jack_nframes_t n = jack_midi_get_event_count(midi_in_buf);
        for (jack_nframes_t i = 0; i < n; i++) {
            jack_midi_event_t event;
            jack_midi_event_get(&event, midi_in_buf, i);

            for (int t=0; t<NB_TRACKS; t++) {
                if (record_mask & (1 << t)) {
                    // queue_push event FIXME
                }
            }
            to_erl(event.buffer, event.size, in, stamp);

            /* Interpret event for local control, e.g. set recorder
             * masks. */
        }
    }

    if (to_erl_buf_bytes) {
        //LOG("buf_bytes = %d\n", (int)to_erl_buf_bytes);
        assert_write(1, to_erl_buf, to_erl_buf_bytes);
    }

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
