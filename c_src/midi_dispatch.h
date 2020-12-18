#ifndef MIDI_DISPATCH_H
#define MIDI_DISPATCH_H

/* How to make dispatch fast while keeping it generic?
   Two things help:
   - use masked addressing
   - use hierarchy (tree search vs. list search)

   Since a normal MIDI event fits into a single 32 bit word, it's easy
   to express the following common match patterns:

       - (port, channel, cc)   -> cc_val
       - (port, channel, note) -> (num, vol)
       - (port, channel)       -> (note, num, vol)

   Some exceptions:
   - sysex
   - RPN, NRPN

*/
typedef uint32_t midi_tag_t;
struct midi_unpacked {
    midi_tag_t tag;
    uint8_t data[];
}
struct midi_dispatch {
    midi_tag_t tag, mask;
    void (*handle)(void*);
};


#endif
