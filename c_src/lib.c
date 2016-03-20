#include "lib.h"
#include <unistd.h>

int assert_write(int fd, const uint8_t *buf, uint32_t nb) {
    int left = nb;
    while(left > 0) {
        int rv;
        ASSERT((rv = write(fd, buf, left)) > 0);
        buf  += rv;
        left -= rv;
    }
    return nb;
}
