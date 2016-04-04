#include "lib.h"
ssize_t raw_write(int fd, const void *buf, size_t count) {
    return write(fd,buf,count);
}


#define APP(x) if (!strcmp(#x,argv[0])) return x(argc,argv)

int main(int argc, char **argv) {
    // erlang.mk seems to insist on building a single binary, so let's
    // just stick to that and delagate here.
    if (argc < 2) {
        ERROR("usage: %s <program> [args...]\n", argv[0]);
    }
    argc--;argv++;
    char *program = argv[0];
    APP(jack_midi);
    APP(jack_control);
    APP(alsa_midi_in);
    APP(alsa_midi_out);
    APP(alsa_seq_in);
    APP(alsa_seq_out);
    ERROR("unkown program %s\n",program);
}
