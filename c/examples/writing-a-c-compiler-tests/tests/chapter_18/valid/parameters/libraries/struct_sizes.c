
/* Test that we can pass static and automatic structs of every size between 1 and 24 bytes.
 * Pass each size both in a register (when possible) and on the stack. */

#include "struct_sizes.h"
int memcmp(void *s1, void *s2, unsigned long n);

// Pass sizes 1 - 6 in registers, remainders on the stack
int fun0(struct bytesize1 a, struct bytesize2 b, struct bytesize3 c,
         struct bytesize4 d, struct bytesize5 e, struct bytesize6 f,
         struct bytesize7 g, struct bytesize8 h, struct bytesize9 i,
         struct bytesize10 j, struct bytesize11 k, struct bytesize12 l,
         struct bytesize13 m, struct bytesize14 n, struct bytesize15 o,
         struct bytesize16 p, struct bytesize17 q, struct bytesize18 r,
         struct bytesize19 s, struct bytesize20 t, struct bytesize21 u,
         struct bytesize22 v, struct bytesize23 w, struct bytesize24 x,
         unsigned char *a_expected, unsigned char *b_expected,
         unsigned char *c_expected, unsigned char *d_expected,
         unsigned char *e_expected, unsigned char *f_expected,
         unsigned char *g_expected, unsigned char *h_expected,
         unsigned char *i_expected, unsigned char *j_expected,
         unsigned char *k_expected, unsigned char *l_expected,
         unsigned char *m_expected, unsigned char *n_expected,
         unsigned char *o_expected, unsigned char *p_expected,
         unsigned char *q_expected, unsigned char *r_expected,
         unsigned char *s_expected, unsigned char *t_expected,
         unsigned char *u_expected, unsigned char *v_expected,
         unsigned char *w_expected, unsigned char *x_expected) {
    if (memcmp(&a, a_expected, sizeof a)) {
        return 0;
    }

    if (memcmp(&b, b_expected, sizeof b)) {
        return 0;
    }

    if (memcmp(&c, c_expected, sizeof c)) {
        return 0;
    }

    if (memcmp(&d, d_expected, sizeof d)) {
        return 0;
    }

    if (memcmp(&e, e_expected, sizeof e)) {
        return 0;
    }

    if (memcmp(&f, f_expected, sizeof f)) {
        return 0;
    }

    if (memcmp(&g, g_expected, sizeof g)) {
        return 0;
    }

    if (memcmp(&h, h_expected, sizeof h)) {
        return 0;
    }

    if (memcmp(&i, i_expected, sizeof i)) {
        return 0;
    }

    if (memcmp(&j, j_expected, sizeof j)) {
        return 0;
    }

    if (memcmp(&k, k_expected, sizeof k)) {
        return 0;
    }

    if (memcmp(&l, l_expected, sizeof l)) {
        return 0;
    }

    if (memcmp(&m, m_expected, sizeof m)) {
        return 0;
    }

    if (memcmp(&n, n_expected, sizeof n)) {
        return 0;
    }

    if (memcmp(&o, o_expected, sizeof o)) {
        return 0;
    }

    if (memcmp(&p, p_expected, sizeof p)) {
        return 0;
    }

    if (memcmp(&q, q_expected, sizeof q)) {
        return 0;
    }

    if (memcmp(&r, r_expected, sizeof r)) {
        return 0;
    }

    if (memcmp(&s, s_expected, sizeof s)) {
        return 0;
    }

    if (memcmp(&t, t_expected, sizeof t)) {
        return 0;
    }

    if (memcmp(&u, u_expected, sizeof u)) {
        return 0;
    }

    if (memcmp(&v, v_expected, sizeof v)) {
        return 0;
    }

    if (memcmp(&w, w_expected, sizeof w)) {
        return 0;
    }

    if (memcmp(&x, x_expected, sizeof x)) {
        return 0;
    }

    return 1; // success
}

// Pass sizes 7-10 bytes in regs, 1-6 on the stack
int fun1(struct bytesize7 a, struct bytesize8 b, struct bytesize9 c,
         struct bytesize10 d, struct bytesize1 e, struct bytesize2 f,
         struct bytesize3 g, struct bytesize4 h, struct bytesize5 i,
         struct bytesize6 j, unsigned char *a_expected,
         unsigned char *b_expected, unsigned char *c_expected,
         unsigned char *d_expected, unsigned char *e_expected,
         unsigned char *f_expected, unsigned char *g_expected,
         unsigned char *h_expected, unsigned char *i_expected,
         unsigned char *j_expected) {
    if (memcmp(&a, a_expected, sizeof a)) {
        return 0;
    }

    if (memcmp(&b, b_expected, sizeof b)) {
        return 0;
    }

    if (memcmp(&c, c_expected, sizeof c)) {
        return 0;
    }

    if (memcmp(&d, d_expected, sizeof d)) {
        return 0;
    }

    if (memcmp(&e, e_expected, sizeof e)) {
        return 0;
    }

    if (memcmp(&f, f_expected, sizeof f)) {
        return 0;
    }

    if (memcmp(&g, g_expected, sizeof g)) {
        return 0;
    }

    if (memcmp(&h, h_expected, sizeof h)) {
        return 0;
    }

    if (memcmp(&i, i_expected, sizeof i)) {
        return 0;
    }

    if (memcmp(&j, j_expected, sizeof j)) {
        return 0;
    }

    return 1; // success
}

// Pass sizes 11-13 in regs, 1 on the stack
int fun2(struct bytesize11 a, struct bytesize12 b, struct bytesize13 c,
         struct bytesize1 d, unsigned char *a_expected,
         unsigned char *b_expected, unsigned char *c_expected,
         unsigned char *d_expected) {
    if (memcmp(&a, a_expected, sizeof a)) {
        return 0;
    }

    if (memcmp(&b, b_expected, sizeof b)) {
        return 0;
    }

    if (memcmp(&c, c_expected, sizeof c)) {
        return 0;
    }

    if (memcmp(&d, d_expected, sizeof d)) {
        return 0;
    }

    return 1; // success
}

// pass sizes 14-16 in regs, 2 on the stack
int fun3(struct bytesize14 a, struct bytesize15 b, struct bytesize16 c,
         struct bytesize2 d, unsigned char *a_expected,
         unsigned char *b_expected, unsigned char *c_expected,
         unsigned char *d_expected) {
    if (memcmp(&a, a_expected, sizeof a)) {
        return 0;
    }

    if (memcmp(&b, b_expected, sizeof b)) {
        return 0;
    }

    if (memcmp(&c, c_expected, sizeof c)) {
        return 0;
    }

    if (memcmp(&d, d_expected, sizeof d)) {
        return 0;
    }

    return 1; // success
}
