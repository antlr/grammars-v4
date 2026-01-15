/* Test that we can return structs of every size between 1 and 24 bytes. */
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
struct bytesize1 fun1(void);

struct bytesize2 {
    unsigned char arr[2];
};

extern struct bytesize2 globvar_2;
struct bytesize2 fun2(void);

struct bytesize3 {
    unsigned char arr[3];
};

extern struct bytesize3 globvar_3;
struct bytesize3 fun3(void);

struct bytesize4 {
    unsigned char arr[4];
};

extern struct bytesize4 globvar_4;
struct bytesize4 fun4(void);

struct bytesize5 {
    unsigned char arr[5];
};

extern struct bytesize5 globvar_5;
struct bytesize5 fun5(void);

struct bytesize6 {
    unsigned char arr[6];
};

extern struct bytesize6 globvar_6;
struct bytesize6 fun6(void);

struct bytesize7 {
    unsigned char arr[7];
};

extern struct bytesize7 globvar_7;
struct bytesize7 fun7(void);

struct bytesize8 {
    unsigned char arr[8];
};

extern struct bytesize8 globvar_8;
struct bytesize8 fun8(void);

struct bytesize9 {
    unsigned char arr[9];
};

extern struct bytesize9 globvar_9;
struct bytesize9 fun9(void);

struct bytesize10 {
    unsigned char arr[10];
};

extern struct bytesize10 globvar_10;
struct bytesize10 fun10(void);

struct bytesize11 {
    unsigned char arr[11];
};

extern struct bytesize11 globvar_11;
struct bytesize11 fun11(void);

struct bytesize12 {
    unsigned char arr[12];
};

extern struct bytesize12 globvar_12;
struct bytesize12 fun12(void);

struct bytesize13 {
    unsigned char arr[13];
};

extern struct bytesize13 globvar_13;
struct bytesize13 fun13(void);

struct bytesize14 {
    unsigned char arr[14];
};

extern struct bytesize14 globvar_14;
struct bytesize14 fun14(void);

struct bytesize15 {
    unsigned char arr[15];
};

extern struct bytesize15 globvar_15;
struct bytesize15 fun15(void);

struct bytesize16 {
    unsigned char arr[16];
};

extern struct bytesize16 globvar_16;
struct bytesize16 fun16(void);

struct bytesize17 {
    unsigned char arr[17];
};

extern struct bytesize17 globvar_17;
struct bytesize17 fun17(void);

struct bytesize18 {
    unsigned char arr[18];
};

extern struct bytesize18 globvar_18;
struct bytesize18 fun18(void);

struct bytesize19 {
    unsigned char arr[19];
};

extern struct bytesize19 globvar_19;
struct bytesize19 fun19(void);

struct bytesize20 {
    unsigned char arr[20];
};

extern struct bytesize20 globvar_20;
struct bytesize20 fun20(void);

struct bytesize21 {
    unsigned char arr[21];
};

extern struct bytesize21 globvar_21;
struct bytesize21 fun21(void);

struct bytesize22 {
    unsigned char arr[22];
};

extern struct bytesize22 globvar_22;
struct bytesize22 fun22(void);

struct bytesize23 {
    unsigned char arr[23];
};

extern struct bytesize23 globvar_23;
struct bytesize23 fun23(void);

struct bytesize24 {
    unsigned char arr[24];
};

extern struct bytesize24 globvar_24;
struct bytesize24 fun24(void);
