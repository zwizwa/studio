#ifndef PORT_H
#define PORT_H

#include <stdint.h>
#include <stdio.h>

// send things to stdout
void s_u8(uint8_t b);
static inline void s_u16(uint16_t w) { s_u8(w>>8); s_u8(w); }
static inline void s_u32(uint32_t w) { s_u16(w>>16); s_u16(w); }
static inline void s_bytes(uint32_t len, uint8_t *buf) { for(uint32_t i = 0; i<len; i++) s_u8(buf[i]); }
static inline void s_tag_term() { s_u8(131); }
static inline void s_tag_u32()  { s_u8(98); }
static inline void s_tag_nil()  { s_u8(106); }
static inline void s_tag_binary(uint32_t len) { s_u8(109); s_u32(len); } // 5
static inline void s_tag_tuple(uint32_t size) { s_u8(104); s_u8(size); } // 2
static inline void s_tag_list(uint32_t len) { s_u8(108); s_u32(len); } // 5
static inline void s_tag_string(uint32_t len) { s_u8(107); s_u16(len); } // 5

void s_term_tuple_of_binaries(int argc, char **argv);
void s_term_list_of_binaries(int argc, char **argv);
void s_term_list_of_u32(int n, uint32_t *a);
void s_term_binary(int n, uint8_t *a);


#endif//PORT_H
