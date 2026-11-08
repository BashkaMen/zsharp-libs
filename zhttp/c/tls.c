// OpenSSL for zhttp. Sockets stay non-blocking: a WANT_READ / WANT_WRITE comes back to Z#,
// which waits on the event loop and calls again. Pointers cross threads as integers because a
// C pointer cannot sit in a Share value (the pool's mutex).
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>
#include <sys/socket.h>
#include <openssl/bio.h>
#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/x509v3.h>

static _Thread_local char tls_err[512];

static void ensure(void) {
    OPENSSL_init_ssl(OPENSSL_INIT_LOAD_SSL_STRINGS | OPENSSL_INIT_LOAD_CRYPTO_STRINGS, NULL);
}

static void set_err(const char *fallback) {
    unsigned long e = ERR_get_error();
    if (e)
        ERR_error_string_n(e, tls_err, sizeof tls_err);
    else
        snprintf(tls_err, sizeof tls_err, "%s", fallback);
    while (ERR_get_error()) {
    }
}

static int looks_like_ip(const char *host) {
    if (!host || !host[0]) return 0;
    for (const char *p = host; *p; p++) {
        if (!((*p >= '0' && *p <= '9') || *p == '.' || *p == ':')) return 0;
    }
    return 1;
}

static SSL *from_bits(uint64_t bits) { return (SSL *)(uintptr_t)bits; }

uint64_t zs_tls_client_ctx(const char *ca, int32_t verify) {
    ensure();
    SSL_CTX *ctx = SSL_CTX_new(TLS_client_method());
    if (!ctx) {
        set_err("SSL_CTX_new failed");
        return 0;
    }
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    SSL_CTX_set_options(ctx, SSL_OP_NO_COMPRESSION);
    if (verify) {
        SSL_CTX_set_verify(ctx, SSL_VERIFY_PEER, NULL);
        int paths = SSL_CTX_set_default_verify_paths(ctx);
        int extra = 1;
        if (ca && ca[0]) extra = SSL_CTX_load_verify_locations(ctx, ca, NULL);
        if (!extra || (paths != 1 && !(ca && ca[0]))) {
            set_err("could not load CA certificates");
            SSL_CTX_free(ctx);
            return 0;
        }
    } else {
        SSL_CTX_set_verify(ctx, SSL_VERIFY_NONE, NULL);
    }
    return (uint64_t)(uintptr_t)ctx;
}

uint64_t zs_tls_server_ctx(const char *cert, const char *key) {
    ensure();
    SSL_CTX *ctx = SSL_CTX_new(TLS_server_method());
    if (!ctx) {
        set_err("SSL_CTX_new failed");
        return 0;
    }
    SSL_CTX_set_min_proto_version(ctx, TLS1_2_VERSION);
    if (SSL_CTX_use_certificate_file(ctx, cert, SSL_FILETYPE_PEM) != 1 ||
        SSL_CTX_use_PrivateKey_file(ctx, key, SSL_FILETYPE_PEM) != 1 ||
        SSL_CTX_check_private_key(ctx) != 1) {
        set_err("could not load the server certificate");
        SSL_CTX_free(ctx);
        return 0;
    }
    return (uint64_t)(uintptr_t)ctx;
}

void zs_tls_ctx_free(uint64_t ctx) {
    if (ctx) SSL_CTX_free((SSL_CTX *)(uintptr_t)ctx);
}

static SSL *new_ssl(uint64_t ctx_bits, int fd) {
    SSL_CTX *ctx = (SSL_CTX *)(uintptr_t)ctx_bits;
    if (!ctx) return NULL;
    SSL *ssl = SSL_new(ctx);
    if (!ssl) return NULL;
    if (SSL_set_fd(ssl, fd) != 1) {
        SSL_free(ssl);
        return NULL;
    }
    SSL_set_mode(ssl, SSL_MODE_ENABLE_PARTIAL_WRITE | SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER);
    SSL_set_quiet_shutdown(ssl, 1);
    BIO *rbio = SSL_get_rbio(ssl);
    BIO *wbio = SSL_get_wbio(ssl);
    if (rbio) BIO_set_nbio(rbio, 1);
    if (wbio) BIO_set_nbio(wbio, 1);
    return ssl;
}

uint64_t zs_tls_client(uint64_t ctx, int32_t fd, const char *host) {
    SSL *ssl = new_ssl(ctx, (int)fd);
    if (!ssl) {
        set_err("SSL_new failed");
        return 0;
    }
    if (host && host[0]) {
        // an address has no SNI; the name is still checked against the certificate
        if (!looks_like_ip(host)) SSL_set_tlsext_host_name(ssl, host);
        SSL_set1_host(ssl, host);
    }
    SSL_set_connect_state(ssl);
    return (uint64_t)(uintptr_t)ssl;
}

uint64_t zs_tls_server(uint64_t ctx, int32_t fd) {
    SSL *ssl = new_ssl(ctx, (int)fd);
    if (!ssl) {
        set_err("SSL_new failed");
        return 0;
    }
    SSL_set_accept_state(ssl);
    return (uint64_t)(uintptr_t)ssl;
}

void zs_tls_free(uint64_t ssl) {
    if (ssl) SSL_free(from_bits(ssl));
}

// 1 done, 2 want read, 3 want write, 0 failed (zs_tls_last_error)
int32_t zs_tls_handshake(uint64_t ssl_bits, int32_t server) {
    SSL *ssl = from_bits(ssl_bits);
    if (!ssl) {
        snprintf(tls_err, sizeof tls_err, "no ssl");
        return 0;
    }
    ERR_clear_error();
    int r = server ? SSL_accept(ssl) : SSL_connect(ssl);
    if (r == 1) {
        if (SSL_get_verify_mode(ssl) != SSL_VERIFY_NONE) {
            long v = SSL_get_verify_result(ssl);
            if (v != X509_V_OK) {
                snprintf(tls_err, sizeof tls_err, "certificate verify failed: %s", X509_verify_cert_error_string(v));
                return 0;
            }
        }
        return 1;
    }
    int err = SSL_get_error(ssl, r);
    if (err == SSL_ERROR_WANT_READ) return 2;
    if (err == SSL_ERROR_WANT_WRITE) return 3;
    long v = SSL_get_verify_result(ssl);
    if (v != X509_V_OK && SSL_get_verify_mode(ssl) != SSL_VERIFY_NONE)
        snprintf(tls_err, sizeof tls_err, "certificate verify failed: %s", X509_verify_cert_error_string(v));
    else
        set_err("handshake failed");
    return 0;
}

// bytes, 0 on a clean close, -2 want read, -3 want write, -1 failed
int32_t zs_tls_read(uint64_t ssl_bits, uint8_t *buf, intptr_t n) {
    SSL *ssl = from_bits(ssl_bits);
    if (!ssl || n <= 0) return 0;
    if (n > 0x7fffffff) n = 0x7fffffff;
    ERR_clear_error();
    int r = SSL_read(ssl, buf, (int)n);
    if (r > 0) return (int32_t)r;
    int err = SSL_get_error(ssl, r);
    if (err == SSL_ERROR_WANT_READ) return -2;
    if (err == SSL_ERROR_WANT_WRITE) return -3;
    if (err == SSL_ERROR_ZERO_RETURN) return 0;
    set_err("read failed");
    return -1;
}

int32_t zs_tls_write(uint64_t ssl_bits, const uint8_t *buf, intptr_t n) {
    SSL *ssl = from_bits(ssl_bits);
    if (!ssl || n <= 0) return 0;
    if (n > 0x7fffffff) n = 0x7fffffff;
    ERR_clear_error();
    int r = SSL_write(ssl, buf, (int)n);
    if (r > 0) return (int32_t)r;
    int err = SSL_get_error(ssl, r);
    if (err == SSL_ERROR_WANT_READ) return -2;
    if (err == SSL_ERROR_WANT_WRITE) return -3;
    set_err("write failed");
    return -1;
}

const char *zs_tls_last_error(void) {
    if (!tls_err[0]) return "tls failed";
    return tls_err;
}

// 1 when the peer has sent nothing and the socket is still open
int32_t zs_fd_idle(int32_t fd) {
    char b;
    ssize_t n = recv((int)fd, &b, 1, MSG_PEEK | MSG_DONTWAIT);
    if (n < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) return 1;
    return 0;
}
