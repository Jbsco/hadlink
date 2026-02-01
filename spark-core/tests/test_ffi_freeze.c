/*  hadlink - High-assurance URL shortener
 *  Copyright (C) 2026 hadlink contributors
 *
 *  SPARK FFI freeze test
 *  Asserts the frozen API contract: version, signatures, return codes.
 *  If this test fails to compile or any assertion fires, the API has changed.
 */

#include <assert.h>
#include <stdio.h>
#include <string.h>

/* FFI declarations (must match core_ffi.ads exports) */
extern void     hadlink_init(void);
extern void     hadlink_final(void);
extern unsigned hadlink_api_version(void);
extern int      hadlink_canonicalize(const char *input, char *output, unsigned long *output_len);
extern int      hadlink_make_short_code(const char *url, const char *secret, char *output);

int main(void) {
    hadlink_init();

    /* 1. API version must be 1 */
    assert(hadlink_api_version() == 1);

    /* 2. Canonicalize a valid URL */
    {
        char output[2049] = {0};
        unsigned long output_len = 0;
        int rc = hadlink_canonicalize("https://example.com/path", output, &output_len);
        assert(rc == 0);                          /* Success */
        assert(output_len == 24);                 /* strlen("https://example.com/path") */
        assert(strcmp(output, "https://example.com/path") == 0);
    }

    /* 3. Canonicalize rejects private address */
    {
        char output[2049] = {0};
        unsigned long output_len = 0;
        int rc = hadlink_canonicalize("http://127.0.0.1/foo", output, &output_len);
        assert(rc == 4);                          /* FFI_Private_Address */
    }

    /* 4. Make short code with 32-byte secret */
    {
        char code[9] = {0};
        const char *secret = "abcdefghijklmnopqrstuvwxyz012345";  /* 32 bytes */
        int rc = hadlink_make_short_code("https://example.com/path", secret, code);
        assert(rc == 0);
        assert(strlen(code) == 8);

        /* Determinism: same inputs produce same output */
        char code2[9] = {0};
        rc = hadlink_make_short_code("https://example.com/path", secret, code2);
        assert(rc == 0);
        assert(strcmp(code, code2) == 0);
    }

    hadlink_final();

    printf("SPARK FFI freeze test: PASSED\n");
    return 0;
}
