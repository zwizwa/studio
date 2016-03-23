#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <unistd.h>
#include <sys/mman.h>

/* Control interface is separate from midi to allow flow control and
   return values. */

#define CMD_CONNECT 1
struct {
    uint8_t len;
    uint8_t cmd;
    uint8_t data[255];
} cmd;

static jack_client_t          *client = NULL;

int jack_control(int argc, char **argv) {
    ASSERT(argc == 2);
    const char *client_name = argv[1];
    jack_status_t status;
    client = jack_client_open (client_name, JackNullOption, &status);

    //jack_set_process_callback (client, process, 0); //necessary?
    ASSERT(!mlockall(MCL_CURRENT | MCL_FUTURE));
    ASSERT(!jack_activate(client));
    for(;;) {
        ssize_t len;
        if ((len = read(0,&cmd,sizeof(cmd))) < 1) exit(len);
        if (!cmd.len) exit(0);
        switch(cmd.cmd) {
            case CMD_CONNECT: {
                char *src = (char*)(cmd.data);
                char *dst = src + strlen(src) + 1;
                LOG("connect %s %s\n", src, dst);
                jack_connect(client, src, dst);
                cmd.len = 0;
                break;
            }
            default:
                LOG("unknown %d (%d)\n", cmd.cmd, cmd.len);
                cmd.len = 0;
                break;
            }
        assert_write(1,(void*)&cmd,1+cmd.len);
    }
}
