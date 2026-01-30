/* Test that we can pass static and automatic structs of every size between 1 and 24 bytes.
 * Pass each size both in a register (when possible) and on the stack. */

#ifdef SUPPRESS_WARNINGS
#ifdef __clang__
#pragma clang diagnostic ignored "-Wincompatible-library-redeclaration"
#else
#pragma GCC diagnostic ignored "-Wbuiltin-declaration-mismatch"
#endif
#endif

struct bytesize1 {
    unsigned char arr[1];
};

extern struct bytesize1 globvar_1;

struct bytesize2 {
    unsigned char arr[2];
};

extern struct bytesize2 globvar_2;

struct bytesize3 {
    unsigned char arr[3];
};

extern struct bytesize3 globvar_3;

struct bytesize4 {
    unsigned char arr[4];
};

extern struct bytesize4 globvar_4;

struct bytesize5 {
    unsigned char arr[5];
};

extern struct bytesize5 globvar_5;

struct bytesize6 {
    unsigned char arr[6];
};

extern struct bytesize6 globvar_6;

struct bytesize7 {
    unsigned char arr[7];
};

extern struct bytesize7 globvar_7;

struct bytesize8 {
    unsigned char arr[8];
};

extern struct bytesize8 globvar_8;

struct bytesize9 {
    unsigned char arr[9];
};

extern struct bytesize9 globvar_9;

struct bytesize10 {
    unsigned char arr[10];
};

extern struct bytesize10 globvar_10;

struct bytesize11 {
    unsigned char arr[11];
};

extern struct bytesize11 globvar_11;

struct bytesize12 {
    unsigned char arr[12];
};

extern struct bytesize12 globvar_12;

struct bytesize13 {
    unsigned char arr[13];
};

extern struct bytesize13 globvar_13;

struct bytesize14 {
    unsigned char arr[14];
};

extern struct bytesize14 globvar_14;

struct bytesize15 {
    unsigned char arr[15];
};

extern struct bytesize15 globvar_15;

struct bytesize16 {
    unsigned char arr[16];
};

extern struct bytesize16 globvar_16;

struct bytesize17 {
    unsigned char arr[17];
};

extern struct bytesize17 globvar_17;

struct bytesize18 {
    unsigned char arr[18];
};

extern struct bytesize18 globvar_18;

struct bytesize19 {
    unsigned char arr[19];
};

extern struct bytesize19 globvar_19;

struct bytesize20 {
    unsigned char arr[20];
};

extern struct bytesize20 globvar_20;

struct bytesize21 {
    unsigned char arr[21];
};

extern struct bytesize21 globvar_21;

struct bytesize22 {
    unsigned char arr[22];
};

extern struct bytesize22 globvar_22;

struct bytesize23 {
    unsigned char arr[23];
};

extern struct bytesize23 globvar_23;

struct bytesize24 {
    unsigned char arr[24];
};

extern struct bytesize24 globvar_24;

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
         unsigned char *w_expected, unsigned char *x_expected);

// Pass sizes 7-10 bytes in regs, 1-6 on the stack
int fun1(struct bytesize7 a, struct bytesize8 b, struct bytesize9 c,
         struct bytesize10 d, struct bytesize1 e, struct bytesize2 f,
         struct bytesize3 g, struct bytesize4 h, struct bytesize5 i,
         struct bytesize6 j, unsigned char *a_expected,
         unsigned char *b_expected, unsigned char *c_expected,
         unsigned char *d_expected, unsigned char *e_expected,
         unsigned char *f_expected, unsigned char *g_expected,
         unsigned char *h_expected, unsigned char *i_expected,
         unsigned char *j_expected);

// Pass sizes 11-13 in regs, 1 on the stack
int fun2(struct bytesize11 a, struct bytesize12 b, struct bytesize13 c,
         struct bytesize1 d, unsigned char *a_expected,
         unsigned char *b_expected, unsigned char *c_expected,
         unsigned char *d_expected);

// pass sizes 14-16 in regs, 2 on the stack
int fun3(struct bytesize14 a, struct bytesize15 b, struct bytesize16 c,
         struct bytesize2 d, unsigned char *a_expected,
         unsigned char *b_expected, unsigned char *c_expected,
         unsigned char *d_expected);
