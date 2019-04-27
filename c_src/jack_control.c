#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/session.h>
#include <unistd.h>
#include <sys/mman.h>

/* Control interface is separate from midi to allow flow control and
   return values.

   http://jackaudio.org/api/
*/

#define CMD_CONNECT 1
struct {
    uint8_t len;
    uint8_t cmd;
    uint8_t data[255];
} cmd;

static jack_client_t          *client = NULL;


//http://jackaudio.org/api/group__ClientCallbacks.html

// FIXME: Send these back to erlang and remove the parsing of jackd output.

static void port_register(jack_port_id_t a, int reg, void *arg) {
    jack_port_t *pa = jack_port_by_id(client, a);
    const char *na = jack_port_name(pa);
    LOG("port_register %s %d\n", na, reg);
}

static void port_connect(jack_port_id_t a, jack_port_id_t b, int connect, void *arg) {
    jack_port_t *pa = jack_port_by_id(client, a);
    jack_port_t *pb = jack_port_by_id(client, b);
    const char *na = jack_port_name(pa);
    const char *nb = jack_port_name(pb);
    LOG("port_connect %s %s %d\n", na, nb, connect);
}
static void client_registration(const char *name, int reg, void *arg) {
    LOG("client_registration %s %d\n", name, reg);
}

// FIXME: There doesn't seem to be a way to catch "scan" events for
// midi devices, that appear on the jackd output.

int jack_control(int argc, char **argv) {
    ASSERT(argc == 2);
    const char *client_name = argv[1];
    jack_status_t status;
    client = jack_client_open (client_name, JackNullOption, &status);

    ASSERT(0 == jack_set_port_registration_callback(client, port_register, NULL));
    ASSERT(0 == jack_set_port_connect_callback(client, port_connect, NULL));
    ASSERT(0 == jack_set_client_registration_callback(client, client_registration, NULL));

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
                // LOG("connect %s %s\n", src, dst);
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
