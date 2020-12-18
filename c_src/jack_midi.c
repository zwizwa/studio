#define _DEFAULT_SOURCE

#include "lib.h"
#include "sysex.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>
#include <pthread.h>
#include <semaphore.h>

/* MIDI I/O

MIDI data is considered "data plane", since Erlang has too high
latency.  Typically we run jack at 48kHz, 64 frames, with MIDI snapped
to the jack frame rate, giving a 750Hz control rate.

Erlang still has access to MIDI in/out.  For some specific controls,
the 1-10ms Erlang latency might be a good enough.

This leaves things open: MIDI (hardware) for low latency, and Erlang
for any kind of network-connected structure.

Inside the jack process we operate:
- Clock timing
- Sequencing
- The low-priority "sysex channel"

I'm currently evaluating 2 designs.  The first one is simpler and
seems to work out:

- low pri input thread (= main)
- real time jack process thread (in poll, direct single write() out)

I'm not exactly sure why this works.  Maybe a single short (1 page)
write to a pipe is about the same complexity from a semaphore post.
I've definitely seen this cause issues when the write size gets
larger.

In any case, there is a second option, with more direct control over
buffering:

- low pri input thread (= main)
- real time jack process thread (in poll, semaphore out)
- low pri output thread (semaphore read)



The low and high priority processes communicate using using single
reader / signle writer lock free queues with fixed memory allocation.

The input thread will throttle incoming data.

The real time thread will drop data that does not fit in the output
queue.

Practical tests will need to indicate what would be good buffer sizes.


Sysex is used to provide a streaming channel between Erlang and the
real time thread, useful for transferring chunked data.  Flow control
is handled through the delay in the input thread, and by chunking data
in the real time thread.

*/



/* Buffer specs.  FIXME: These could be made configurable at program
   startup to allow tuning without recompilation. */

/* Input */
#define NB_FROM_ERL_BUFS 8
#define MIDI_CMD_SIZE (4 + 64)

/* Output */
#define TO_ERL_SIZE_LOG 12
//#define TO_ERL_SIZE_LOG 6 // to test chunking
#define TO_ERL_SIZE (1 << TO_ERL_SIZE_LOG)


static int nb_midi_out; static jack_port_t **midi_out = NULL;
static int nb_midi_in;  static jack_port_t **midi_in  = NULL;
static jack_client_t          *client = NULL;

#define BPM_TO_PERIOD(sr,bpm) ((sr*60)/(bpm*24))

typedef uint32_t mask_t;

static jack_nframes_t clock_period = BPM_TO_PERIOD(48000, 120);
static jack_nframes_t clock_time;
static mask_t clock_mask = 0;


/* Erlang incoming is only MIDI.  We use sysex to tunnel any
   non-standard data.

   As an illustration, this is the format used by Roland:

   F0 sysex start
   41 manufacturer code
   :1 device id
   :1 model id
   :1 request(11) or send(12)
   :1 address, object identifier
   :* bulk data or number of bytes requested
   41 checksum
   F7 sysex end

   For our purposes:

   - We can use a virtual port to send/receive sysex, so the message
     itself does not need to contain any identifiers that would be
     used to perform further routing.

   - Checksums are likely not necessary since transport between Erlang
     and the port process is reliable.

   - Any other semantics can be encoded inside the payload binary.

   - It's ok to make this unidirectional, implementing the two
     directions separately.  Any bi-directional communication can be
     built on top of that.

   - It can be assumed that all messages in a sysex stream chunk are
     ordered and part of the same high level message.  Mutual
     exclusion will need to be handled at the Erlang level.

   This gives our simplified format:
   F0 sysex start
   .. manufacturer code
   .. decoder info
   :* bulk data in sysex.h 7<->8 byte enc/dec format
   F7 sysex end

   The decoder info tag can be used to assist assembly of multiple
   packets.

   For chunking it is convenient to use multiples of 8 bytes in the
   bulk data, meaning the necessary buffer size is 4 + n * 8, where n
   is the number of 8 byte packets.

*/
struct command {
    uint32_t size;  // {packet,4}
    mask_t   mask;  // Allow broadcasting
    uint8_t  midi_bytes[MIDI_CMD_SIZE];
} __attribute__((__packed__));
static struct command from_erl_buf[NB_FROM_ERL_BUFS];

static volatile unsigned int from_erl_read = 0, from_erl_write = 0;

static inline uint32_t command_nb_midi_bytes(struct command *c) {
    return c->size - sizeof(mask_t);
}

#define CONTROL_PORT 31


/* Erlang outgoing is also only MIDI.

   All midi data is collected in a single buffer to allow spending
   only a singlewrite() syscall in the process thread.

   This seems ok as long as the output size is small.  I'm guessing
   that a single 4K page would be appropriate.  For standard 48kHz, 64
   frame operation, that gives a 3MByte/sec transfer rate which should
   be enough.
*/

static uint8_t to_erl_buf[TO_ERL_SIZE];
static size_t to_erl_buf_bytes = 0;

uint32_t to_erl_room(void) {
    uint32_t free_bytes = sizeof(to_erl_buf) - to_erl_buf_bytes;
    if (free_bytes >= 6) return free_bytes - 6;
    return 0;
}

static uint8_t *to_erl_hole(int nb, uint8_t port, uint8_t stamp) {
    size_t msg_size = 6 + nb;
    if (to_erl_buf_bytes + msg_size > sizeof(to_erl_buf)) {
        LOG("midi buffer overflow\n");
        return NULL;
    }
    uint8_t *msg = &to_erl_buf[to_erl_buf_bytes];
    set_u32be(msg, msg_size - 4); // {packet,4}
    msg[4] = port;                // MIDI port number
    msg[5] = stamp;               // rolling time stamp  (FIXME: use clock_time?)
    to_erl_buf_bytes += msg_size;
    return &msg[6];
}


static void to_erl(const uint8_t *buf, int nb, uint8_t port, uint8_t stamp) {
    uint8_t *hole = to_erl_hole(nb, port, stamp);
    if (hole) { memcpy(hole, buf, nb); }
}



/* State dump.

   To avoid mutual exclusion issues, send it from the process thread.
   CPU usage is not the issue, but we do need to be careful about the
   data size sent from the process thread.  So we buffer the dump,
   then send it out in chunks.

   The protocol is essentially tagged s-expressions. This solves two
   problems: no 8 bit -> 7 bit encoding is necessary, and a little
   more robustness is created in case the protocol evolves in the
   future.  Flat binary formats are a pain to maintain, and
   s-expressions are fairly simple to generate be it about 3 times
   less efficient.
*/


static uint8_t dump_buf[1024 * 64];
static uint32_t dump_buf_write, dump_buf_read;
static void dump_start(void) {
    dump_buf_write = 0;
    dump_buf_read = 0;
}
static uint32_t dump_end(void) {
    return dump_buf_write;
}
static void dump_byte(uint8_t c) {
    if (dump_buf_write < sizeof(dump_buf)) {
        dump_buf[dump_buf_write++] = c;
    }
}
static uint8_t dump_last_byte(void) {
    if (dump_buf_write == 0) return 0;
    return dump_buf[dump_buf_write-1];
}

#define NS(name) CONCAT(dump,name)
#include "ns_dump.h"
#undef NS


/* Sequencer.

   The sequencer is based on a collection of rolling queues, one for
   each cycle length.  Fixed size items and power of 2 buffer sizes
   are used to simplify code.
*/
#define EVENT_MIDI_SIZE 4
struct event {
    uint16_t rel_phase;
    uint8_t nb_bytes;
    uint8_t port;
    uint8_t midi[EVENT_MIDI_SIZE];
} __attribute__((__packed__));




static inline void dump_event(struct event *e) {
    dump_open("e");
    dump_number(e->rel_phase);
    dump_number(e->port);
    dump_open("m");
    for(int i=0;i<e->nb_bytes;i++) {
        dump_number(e->midi[i]);
    }
    dump_close();
    dump_close();
}

// Keep these a power of two, such that % can be optimized to &.
#define NB_EVENTS (1<<8)
struct event_queue {
    struct event buf[NB_EVENTS];
    uint32_t read;
    uint32_t write;
};
// The queue.h module is parameterized by a NS namespace macro.
// It requires the following types to be defined:
#define NS(name) CONCAT(event_queue,name)
typedef struct event        event_queue_element_t;
typedef struct event_queue  event_queue_container_t;
#include "ns_queue.h"
#undef NS

static inline void dump_event_queue(struct event_queue *q) {
    for (uint32_t p = q->read; p != q->write; p = (p + 1) % NB_EVENTS) {
        dump_event(&q->buf[p]);
    }
}



#define NB_EDITS (1<<8)
struct edit {
    uint8_t edit_type; // add/remove
    uint8_t track;
    uint16_t _reserved;
    struct event event;
} __attribute__((__packed__));
struct edit_queue {
    struct edit buf[NB_EDITS];
    uint32_t read;
    uint32_t write;
};
struct edit_queue edit_queue;
#define NS(name) CONCAT(edit_queue,name)
typedef struct edit       edit_queue_element_t;
typedef struct edit_queue edit_queue_container_t;
#include "ns_queue.h"
#undef NS





/* The sequencer is under midi control.  Each loop has an input
   mask for port events.  Recording is gated.  Start with 9 sequences
   mapped to cc23-cc31, corresponding to the Easycontrol 9. */
struct track {
    struct event_queue q;
    uint16_t abs_phase;
    uint16_t period;
    uint32_t port_mask;
};

/* Event time base is relative to the loop length using 16-bit
 * subdivision of the loop.  This makes it easier to change loop sizes
 * on-the-fly.  Tracks currently use absolute units expressed in jack
 * frames.  These functions perform the conversion and keeps the
 * definition of the relation in one place. */
static inline uint32_t abs_phase(const struct track *t, uint16_t rel_phase) {
    return (t->period * rel_phase) >> 16;
}
static inline uint32_t rel_phase(const struct track *t, uint16_t abs_phase) {
    return  (((uint32_t)abs_phase) << 16) / ((uint32_t)t->period);
}


static inline void track_tick(struct track *t) {
    //LOG("phase %d\n", t->phase);
    if (!t->period) return; // FIXME
    t->abs_phase = (t->abs_phase + 1) % t->period;
}
// FIXME: this creates an infinite loop in case there is only one
// phase recorded, so do it in two steps: get the nb of elements to
// send out and perform this loop a number of times.  or, just make it
// an iterator. that would remove the ambiguity.


typedef void (*play_t)(void *ctx, uint8_t port, const void *midi, size_t nb_bytes);
static inline void track_playback(struct track *t, play_t play, void *ctx) {
    uint32_t endx = t->q.write;
    for(;;) {
        const struct event *pe = event_queue_peek(&t->q);
        if (!pe || (t->abs_phase != abs_phase(t, pe->rel_phase))) break;
        // don't cycle more than once
        if (t->q.read == endx) break;
        // there is an event left at this time phase, so play and cycle the queue
        play(ctx, pe->port, &pe->midi[0], pe->nb_bytes);
        event_queue_shift(&t->q);
    }
    /* Advance time base once per Jack frame. */
    track_tick(t);
}
static inline void track_record_event(struct track *t, const struct event *e) {
    event_queue_write(&t->q, e);
}
static inline void track_record(struct track *t, uint8_t port,
                                uint8_t *midi, int32_t nb_bytes) {
    struct event e = {
        .rel_phase = rel_phase(t, t->abs_phase),
        .nb_bytes = nb_bytes,
        .port = port
    };
    if (nb_bytes) memcpy(&e.midi[0], &midi[0], nb_bytes);
    track_record_event(t, &e);
}

/* If record bit is set for a particular track, and the track's
 * port_mask is valid for the current port, record the event. */
uint32_t record_mask = 0;
uint32_t playback_mask = -1;

#define NB_TRACKS 9
struct track track[NB_TRACKS];

void tracks_init(void) {
    // 4 bards at 120bpm is 2 seconds
    uint32_t period = (48000*2) / 64;
    for(int t=0; t<NB_TRACKS; t++) {
        track[t].period = period;
    }
}

void dump_track(struct track *t) {
    dump_open("t");
    dump_number(t->period);
    dump_event_queue(&t->q);
    dump_close();
}




// Send midi data out over a jack port.
static inline void send_midi(void *out_buf, jack_nframes_t time,
                             const void *data_buf, size_t nb_bytes) {
    //LOG("%d %d %d\n", frames, time, (int)nb_bytes);
    void *buf = jack_midi_event_reserve(out_buf, time, nb_bytes);
    if (buf) memcpy(buf, data_buf, nb_bytes);
}
// Lambda-lifted closure to be used with track_playback.  Data is sent
// out over the midi ports and Erlang.  FIXME: There should be some
// input->output routing here, as it is assumed that each instrument
// just receives its own data back, which is not the general case.
struct play_midi_ctx {
    void **midi_out_buf;
    jack_nframes_t time;
    uint8_t stamp;
};
static void play_midi_fun(void *ctx, uint8_t port,
                          const void *data_buf, size_t nb_bytes) {
    struct play_midi_ctx *x = ctx;
    send_midi(x->midi_out_buf[port], x->time, data_buf, nb_bytes);
    to_erl(data_buf, nb_bytes, port, x->stamp);
}


void log_buf(const char *name, uint8_t *buf, uint32_t len) {
    LOG("%s",name); for (int i=0; i<len; i++) { LOG(" %02x", buf[i]); } LOG("\n");
}

// FIXME: This uses asserts for protocol errors since it is more
// convenient.  Maybe later replace them with non-fatal logging
// ignores.
static inline void process_dec_sysex(uint8_t *buf, uint32_t nb_bytes) {
    // log_buf("decoded sysex: ", buf, nb_bytes);
    ASSERT(nb_bytes >= 4);
    uint32_t cmd = *((uint32_t*)buf);
    buf += 4;
    nb_bytes -= 4;
    uint32_t *arg = (void*)buf;
    switch(cmd) {
    case 1: {
        /* INSERT: Insert an event in a track.  This goes through the
         * edit queue, which will insert the event on the next cycle
         * passing through. */
        ASSERT(nb_bytes == sizeof(struct edit));
        struct edit *edit = (void*)buf;
        ASSERT(edit->event.nb_bytes <= EVENT_MIDI_SIZE);
        LOG("enqueue edit\n");
        edit_queue_write(&edit_queue, edit);
        break;
    }
    case 2: {
        /* CLEAR: Clear current track.  Individual removals are not
         * supported.  To do so, clear and reload. */
        ASSERT(nb_bytes == 4);
        uint32_t track_nb = arg[0] % NB_TRACKS;
        LOG("clear track %d\n", track_nb);
        event_queue_clear(&track[track_nb].q);
        break;
    }
    case 3: {
        /* DUMP: Take a state snapshot to maintain transaction
         * semantics.  The snapshot will be transferred
         * incrementally. */
        ASSERT(nb_bytes == 4);
        uint32_t track_nb = arg[0] % NB_TRACKS;
        ASSERT(dump_buf_read == dump_buf_write);
        dump_start();
        dump_track(&track[track_nb]);
        dump_end();
        uint32_t len = dump_end();
        (void)len;
        //log_buf("dump buf: ", dump_buf, len);
        break;
    }
    case 4: {
        /* SET LOOP PERIOID */
        ASSERT(nb_bytes == 8);
        uint32_t track_nb = arg[0] % NB_TRACKS;
        uint32_t period   = arg[1];
        ASSERT(period > 0);
        track[track_nb].period = period;
        break;
    }
    default:
        break;
    }
}

static inline void process_erl_in(void **midi_out_buf) {

    /* Jack requires us to sort the events, so send the async data
       first using time stamp 0. */
    while(from_erl_read != from_erl_write) {
        struct command *cmd = &from_erl_buf[from_erl_read];
        uint32_t nb_midi = command_nb_midi_bytes(cmd);
        uint8_t *midi = &cmd->midi_bytes[0];

        /* Send to selected outputs */
        for (int out=0; out<nb_midi_out; out++) {
            if (cmd->mask & (1 << out)) {
                // LOG("send_midi: %d\n", nb_midi_bytes);
                send_midi(midi_out_buf[out], 0/*time*/,
                          midi, nb_midi);
            }
        }

        /* Handle sysex control commands.  Note that in Erlang we
         * could easily define another protocol to send non-MIDI data.
         * However, embedding the custom protocol in sysex allows it
         * to be used over MIDI-only links, keeping more options open.
         * I.e. I'd like to be able to put this code on an STM32. */
        if ((cmd->mask & (1 << CONTROL_PORT)) &&
            (nb_midi > 3) &&
            (midi[0] == 0xF0) &&
            (midi[nb_midi-1] == 0xF7)) {

            // FIXME: For ordinary MIDI links, the real-time messages
            // should be stripped out.

            // Framing is ok.  Interpret the data.
            uint32_t nb_enc = nb_midi - 3;
            int32_t nb_dec = sysex_decode_size(nb_enc);
            ASSERT(nb_dec >= 0);
            uint8_t dec[nb_dec];
            memset(dec, 0, sizeof(dec));
            sysex_decode(dec, midi + 2, nb_enc);

            process_dec_sysex(&dec[0], nb_dec);

        }

        from_erl_read = (from_erl_read + 1) % NB_FROM_ERL_BUFS;
    }

}

static inline void process_edit(void **midi_out_buf, uint8_t stamp) {
    /* The sequencer queue implementation does not not have random
     * insert, so we delay external edits and insert them once the
     * queue is wound to the correct time stamp.  Play them back at
     * that time as well.  Note that this requires the events in the
     * edit queue to be ordered relative to current time for each loop
     * sequencer's time base, or they will be delayed until next
     * time. */
    jack_nframes_t time = 0;

    for(;;) {
        const struct edit *pe = edit_queue_peek(&edit_queue);
        if (!pe) break; // no edit events
        const struct event *e = &pe->event;

        ASSERT(pe->track < NB_TRACKS);
        struct track *t = &track[pe->track];

        uint32_t phase = abs_phase(t, e->rel_phase);

        if (t->abs_phase != phase) break; // edit event is in the future

        /* The edit event is current, so record it into the track and
           play it back. */
        track_record_event(t, e);
        send_midi(midi_out_buf[e->port], time, &e->midi[0], e->nb_bytes);
        edit_queue_drop(&edit_queue);
    }
}


static inline void process_playback_out(void **midi_out_buf, uint8_t stamp) {
    /* Play back sequences.  Send these at time stamp 0 as well. */
    static struct play_midi_ctx play_midi_ctx = {};
    play_midi_ctx.midi_out_buf = &midi_out_buf[0];
    play_midi_ctx.stamp = stamp;
    for (int t=0; t<NB_TRACKS; t++) {
        track_playback(&track[t], &play_midi_fun, &play_midi_ctx);
    }
}

static inline void process_clock_out(void **midi_out_buf, jack_nframes_t nframes, uint8_t stamp) {
    /* Send out the MIDI clock bytes at the designated time slots */
    while(clock_time < nframes) {
        /* Clock pulse fits in current frame. */
        const uint8_t clock[] = {0xF8};
        for (int out=0; out<nb_midi_out; out++) {
            if (clock_mask & (1 << out)) {
                //LOG("F8 on %d\n", out);
                send_midi(midi_out_buf[out], clock_time, clock, sizeof(clock));
            }
        }

        /* Send to Erlang for recording. */
        to_erl(&clock[0], sizeof(clock), 0xff, stamp);

        /* Advance clock */
        clock_time += clock_period;
    }
    /* Account for this frame */
    clock_time -= nframes;

}

static inline void process_midi_in(jack_nframes_t nframes, uint8_t stamp) {
    /* Process incoming midi */


    for (int in=0; in<nb_midi_in; in++) {
        void *midi_in_buf  = jack_port_get_buffer(midi_in[in], nframes);
        jack_nframes_t n = jack_midi_get_event_count(midi_in_buf);
        for (jack_nframes_t i = 0; i < n; i++) {
            jack_midi_event_t event;
            jack_midi_event_get(&event, midi_in_buf, i);

            /* Record to MIDI looper track if enabled. */
            for (int t=0; t<NB_TRACKS; t++) {
                if (record_mask & (1 << t)) {
                    //LOG("record t=%d, in=%d, n=%d\n", t, in, (int)event.size);
                    if (event.size < EVENT_MIDI_SIZE) {
                        track_record(&track[t], in, event.buffer, event.size);
                    }
                }
            }

            /* Send a time-stamped copy of everything to Erlang.  This
             * is used for soft-RT events to anything that's not jack
             * midi, and bulk recording to disk. */
            to_erl(event.buffer, event.size, in, stamp);

            /* Local MIDI control.  Currently this only has record
             * enable.  FIXME: Make this programmable. */
            const uint8_t *msg = event.buffer;
            if (in == 0 &&
                event.size == 3 &&
                msg[0] == 0xB0 && // CC channel 0
                (msg[1] >= 23) && // CC num on Easycontrol 9
                (msg[1] <= 31)) {
                uint32_t t = msg[1]-23;
                if (msg[2]) {
                    record_mask |= (1<<t);
                }
                else {
                    record_mask &= ~(1<<t);
                }
                LOG("record %d %d\n", t, msg[2]);
            }
        }
    }
}


static inline void process_erl_out(uint8_t stamp) {

    /* Send to Erlang

       Note: I'm not exactly sure whether it is a good idea to perform
       the write() call from this thread, but it seems the difference
       between a single semaphore system call and a single write to an
       Erlang port pipe accessing a single page of memory is not going
       to be big.  So revisit if it ever becomes a problem.

       As compared to a previous implementation, this will now buffer
       all midi meassages and perform only a single write() call.

    */

    if (0) {
        /* Sysex pressure test.  This seems to not cause any issues
         * for small buffers.  I don't really understand why it works,
         * because I've definitely seen issues when using larger
         * messages (order of 128kByte, for audio). */
        uint8_t m[100] = {};
        m[0] = 0xF0, m[1]=0x60, m[sizeof(m)-1]=0xF7;
        to_erl(&m[0], sizeof(m), CONTROL_PORT, stamp);
    }

    /* Empty the dump buf, sending chunks. */
    uint32_t nb = dump_buf_write - dump_buf_read;
    if (nb) {
        uint32_t room = to_erl_room();
        if (room > 4) { // Needs to fit framing
            uint32_t max_nb = room - 4;
            if (nb > max_nb) nb = max_nb;
            // New version: dump as ASCII s-expression
            uint8_t *hole = to_erl_hole(nb + 4, CONTROL_PORT, stamp);
            hole[0] = 0xF0;
            hole[1] = 0x60;  // Manufacturer (from reserved space)
            hole[nb + 3] = 0xF7;
            memcpy(hole + 3, &dump_buf[dump_buf_read], nb);
            dump_buf_read += nb;
            // 0=last, 1=remaining
            hole[2] = !!(dump_buf_write - dump_buf_read);
        }
    }


    if (to_erl_buf_bytes) {
        //LOG("buf_bytes = %d\n", (int)to_erl_buf_bytes);
        assert_write(1, to_erl_buf, to_erl_buf_bytes);
    }

}

static int process (jack_nframes_t nframes, void *arg) {

    /* Clear output buffers */
    to_erl_buf_bytes = 0;
    void *midi_out_buf[nb_midi_out];
    for (int out=0; out<nb_midi_out; out++) {
        midi_out_buf[out] = jack_port_get_buffer(midi_out[out], nframes);
        jack_midi_clear_buffer(midi_out_buf[out]);
    }

    /* Erlang out is tagged with a rolling time stamp. */
    jack_nframes_t f = jack_last_frame_time(client);
    uint8_t stamp = (f / nframes);

    /* Order is important. */
    process_erl_in(midi_out_buf);
    process_edit(midi_out_buf, stamp);
    process_playback_out(midi_out_buf, stamp);
    process_clock_out(midi_out_buf, nframes, stamp);
    process_midi_in(nframes, stamp);
    process_erl_out(stamp);

    return 0;
}

pthread_t output_thread;
sem_t output_sema;
static void *output_thread_main(void *ctx) {
    for(;;) {
        ASSERT_ERRNO(sem_wait(&output_sema));
    }
}

int jack_midi(int argc, char **argv) {

    ASSERT(argc == 5);

    /* State init */
    tracks_init();

    /* Output thread. */
    pthread_create(&output_thread, NULL, &output_thread_main, NULL);

    /* Jack client setup */
    const char *client_name = argv[1];
    nb_midi_in  = atoi(argv[2]);  midi_in  = calloc(nb_midi_in,sizeof(void*));
    nb_midi_out = atoi(argv[3]);  midi_out = calloc(nb_midi_out,sizeof(void*));
    clock_mask  = atoi(argv[4]);
    LOG("jack_midi.c: clock_mask = %d\n", clock_mask);
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

    /* Input loop. */
    for(;;) {
        while (((from_erl_write - from_erl_read) % NB_FROM_ERL_BUFS)
               == (NB_FROM_ERL_BUFS - 1)) {
            /* Not much that can be done here except for polling until
               there is am available slot.  Tune buffer sizes such
               that this won't happen. */
            LOG("input stall\n");
            usleep(1000);
        }
        struct command *cmd = &from_erl_buf[from_erl_write];
        memset(cmd, 0, sizeof(*cmd));
        assert_read_packet4_static(0, cmd, sizeof(*cmd));

        // LOG("msg: n=%d\n", cmd->size);
        from_erl_write = (from_erl_write + 1) % NB_FROM_ERL_BUFS;
    }
    return 0;
}
