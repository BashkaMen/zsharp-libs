// Pieces libpq's header doesn't give us as Z# values.
//
// A PGconn* can't sit in a Mutex: a C pointer isn't Share, and the pool is used from every
// event loop. The pointer is stored as an integer (the same bits) and turned back at the call.
// A release on one loop wakes a task waiting on another by writing one byte to a socket pair;
// the waiter is in Task.waitFor, so the loop itself isn't blocked and nothing is polled by hand.

#include <errno.h>
#include <locale.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <libpq-fe.h>

#ifdef __APPLE__
#include <sys/types.h>
#endif

intptr_t zpg_ptr_to_int(void *p) { return (intptr_t)p; }

void *zpg_int_to_ptr(intptr_t n) { return (void *)n; }

int zpg_is_null(const void *p) { return p == NULL; }

// The same bytes the Z# string passed. libpq copies them during PQsendQueryParams.
const char *zpg_cstr(const char *s) { return s; }

int zpg_send_params(void *conn, const char *sql, int n, const char *const *values) {
    return PQsendQueryParams((PGconn *)conn, sql, n, NULL, values, NULL, NULL, 0);
}

// The shortest decimal that reads back as the same double (15, 16, then 17 digits), and that
// doesn't follow LC_NUMERIC, so "1.5" stays "1.5" for the server. 17 digits always round-trip.
int zpg_format_float(double x, char *buf, int cap) {
    if (cap <= 0) return -1;
    locale_t c = newlocale(LC_NUMERIC_MASK, "C", (locale_t)0);
    if (!c) return -1;
    locale_t old = uselocale(c);
    int n = -1;
    for (int digits = 15; digits <= 17; digits++) {
        n = snprintf(buf, (size_t)cap, "%.*g", digits, x);
        if (n < 0 || n >= cap || x != x || strtod(buf, NULL) == x) break;
    }
    uselocale(old);
    freelocale(c);
    return n;
}

// Both ends non-blocking and close-on-exec. 0, or -errno.
int zpg_wake_pair(int *r, int *w) {
    int fds[2];
    if (socketpair(AF_UNIX, SOCK_STREAM, 0, fds) != 0) return -errno;
    for (int i = 0; i < 2; i++) {
        int flags = fcntl(fds[i], F_GETFL, 0);
        if (flags < 0 || fcntl(fds[i], F_SETFL, flags | O_NONBLOCK) != 0) {
            int e = errno;
            close(fds[0]);
            close(fds[1]);
            return -e;
        }
        int fdflags = fcntl(fds[i], F_GETFD, 0);
        if (fdflags >= 0) fcntl(fds[i], F_SETFD, fdflags | FD_CLOEXEC);
#ifdef __APPLE__
        int one = 1;
        setsockopt(fds[i], SOL_SOCKET, SO_NOSIGPIPE, &one, sizeof one);
#endif
    }
    *r = fds[0];
    *w = fds[1];
    return 0;
}

// One byte. 0, or -errno (EPIPE when the waiter has already gone).
int zpg_wake(int w) {
    char b = 1;
#ifdef MSG_NOSIGNAL
    if (send(w, &b, 1, MSG_NOSIGNAL) == 1) return 0;
#else
    if (send(w, &b, 1, 0) == 1) return 0;
#endif
    return -errno;
}

// Empties a non-blocking socket. The count is only for the caller to ignore.
int zpg_drain(int r) {
    char buf[64];
    int total = 0;
    for (;;) {
        ssize_t n = recv(r, buf, sizeof buf, 0);
        if (n > 0) total += (int)n;
        else break;
    }
    return total;
}

void zpg_close(int fd) {
    if (fd >= 0) close(fd);
}
