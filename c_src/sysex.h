#ifndef SYSEX_H
#define SYSEX_H

/* Encode / decode to "prefixed MSB byte" format.  The first bit
   corresponds to the first byte, etc.. */

// FIXME: untested!

static inline int32_t sysex_encode_size(uint32_t dec_size) {
    uint32_t q = dec_size / 7;
    uint32_t r = dec_size % 7;
    uint32_t nb_prefixes = q + r ? 1 : 0;
    return nb_prefixes + (7 * q) + r;
}
static inline int32_t sysex_decode_size(uint32_t enc_size) {
    uint32_t q = enc_size / 8;
    uint32_t r = enc_size % 8;
    if (r == 1) r = 0; // ignore spurious MSB byte
    return 7 * q + (r ? r-1 : 0);
}

// Destination buffers are assumed to have correct size.  Use the two
// routines above to obtain size for allocation purposes.

static inline void sysex_decode(uint8_t *dec_buf, const uint8_t *enc_buf, uint32_t enc_len) {
    int e = 0, d = 0;
    for(;;) {
        if (e >= enc_len) return;
        uint8_t msb = enc_buf[e++];
        for(int i=0; i<7; i++) {
            if (e >= enc_len) return;
            dec_buf[d++] = enc_buf[e++] | (0x80 & (msb << (7 - i)));
        }
    }
}
static inline void sysex_encode(uint8_t *enc_buf, const uint8_t *dec_buf, uint32_t dec_len) {
    for(int d = 0; d < dec_len; d++) {
        int q = d / 7;
        int r = d % 7;
        enc_buf[q * 8 + 1 + r] = 0x7f & dec_buf[d];
        enc_buf[q * 8]         = 0x7f & ((enc_buf[q * 8] & ~(1 << r)) | ((dec_buf[d] >> 7) << r));
    }
}



#endif // SYSEX_H
