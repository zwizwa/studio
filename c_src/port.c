#include "port.h"
#include <string.h>
// http://erlang.org/doc/apps/erts/erl_ext_dist.html
// Produce a 4-byte size prefixed encoded term, encoding a small tuple of binaries.
void s_term_tuple_of_binaries(int argc, char **argv) {
    uint32_t len[argc];
    uint32_t len_sum = 0;
    for(int i=0; i<argc; i++) {
        len_sum += (len[i] = argv[i] ? strlen(argv[i]) : 0);
    }
    uint32_t total =
        1 +          /* s_tag_term */
        2 +          /* s_tag_tuple*/
        (5 * argc) + /* s_tag_binary */
        len_sum;     /* binary data */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_tuple(argc);
    for(int i=0; i<argc; i++){
        s_tag_binary(len[i]);
        s_bytes(len[i],(uint8_t*)argv[i]);
    }
}
void s_term_list_of_binaries(int argc, char **argv) {
    uint32_t len[argc];
    uint32_t len_sum = 0;
    for(int i=0; i<argc; i++) {
        len_sum += (len[i] = argv[i] ? strlen(argv[i]) : 0);
    }
    uint32_t total =
        1 +          /* s_tag_term */
        5 +          /* s_tag_list*/
        (5 * argc) + /* s_tag_binary */
        len_sum +    /* binary data */
        1;           /* s_nil */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_list(argc);
    for(int i=0; i<argc; i++){
        s_tag_binary(len[i]);
        s_bytes(len[i],(uint8_t*)argv[i]);
    }
    s_tag_nil();
}
void s_term_list_of_u32(int n, uint32_t *a){
    uint32_t total =
        1 +          /* s_tag_term */
        5 +          /* s_tag_list*/
        5 * n +      /* integer tag + data */
        1;           /* s_nil */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_list(n);
    for(int i=0; i<n; i++){
        s_tag_u32();
        s_u32(a[i]);
    }
    s_tag_nil();
}
void s_term_binary(int n, uint8_t *a){
    uint32_t total =
        1 +          /* s_tag_term */
        5 +          /* s_tag_binary*/
        n;           /* binary data */
    s_u32(total); // {packet,4}
    s_tag_term();
    s_tag_binary(n);
    s_bytes(n,a);
}
