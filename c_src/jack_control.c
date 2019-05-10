#include "lib.h"
#include <jack/jack.h>
#include <jack/midiport.h>
#include <jack/session.h>
#include <unistd.h>
#include <sys/mman.h>

/* Control interface is separate from midi to allow flow control and
   return values.

   Both ends of the pipe are asynchronous.  This makes it easier to
   write directly to stdout from the jack callbacks.

   Protocol is ad-hoc, whatever is easier to parse/generate here.

   Erl -> C is binary
   C -> Erl uses strings

   http://jackaudio.org/api/
*/

#define CMD_CONNECT 1
#define CMD_DISCONNECT 2

static jack_client_t          *client = NULL;


#define SEND(fmt, ...) {\
        char buf[256];                                                 \
        buf[0] = snprintf(buf+1, sizeof(buf)-1,fmt, __VA_ARGS__);      \
        assert_write(1, (void*)buf, buf[0] + 1);                       \
    }

static void port_register(jack_port_id_t a, int reg, void *arg) {
    jack_port_t *pa = jack_port_by_id(client, a);
    const char *na = jack_port_name(pa);

    //LOG("port_register %s %d\n", na, reg);
    SEND("{port,%s,\"%s\"}", reg ? "true" : "false", na);

    char alias0[jack_port_name_size()];
    char alias1[jack_port_name_size()];
    char *const alias[2] = {alias0, alias1};
    int nb_alias = jack_port_get_aliases(pa, alias);
    for (int i = 0; i<nb_alias; i++) {
        //LOG(" - alias: %s\n", alias[i]);
        SEND("{alias,\"%s\",\"%s\"}", na, alias[i]);
    }
}


static void port_connect(jack_port_id_t a, jack_port_id_t b, int connect, void *arg) {
    jack_port_t *pa = jack_port_by_id(client, a);
    jack_port_t *pb = jack_port_by_id(client, b);
    const char *na = jack_port_name(pa);
    const char *nb = jack_port_name(pb);
    //LOG("port_connect %s %s %d\n", na, nb, connect);
    SEND("{connect,%s,\"%s\",\"%s\"}",connect  ? "true" : "false", na, nb);
}
static void client_registration(const char *name, int reg, void *arg) {
    //LOG("client_registration %s %d\n", name, reg);
    SEND("{client,%s,\"%s\"}", reg  ? "true" : "false", name);
}

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
        char buf[255];
        int len = assert_read_packet1(0, &buf);
        ASSERT(len > 0);
        switch(buf[0]) {
            case CMD_CONNECT:
            case CMD_DISCONNECT: {
                char *src = &buf[1];
                char *dst = src + strlen(src) + 1;
                // LOG("connect %s %s\n", src, dst);
                switch(buf[0]) {
                case CMD_CONNECT:
                    jack_connect(client, src, dst);
                    break;
                case CMD_DISCONNECT:
                    jack_disconnect(client, src, dst);
                    break;
                }
                break;
            }
            default:
                LOG("unknown %d (%d)\n", buf[0], len);
                exit(1);
        }
    }
}
