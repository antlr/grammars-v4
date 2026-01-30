/* Test that we can return structs of every size between 1 and 24 bytes. */
#include "retval_struct_sizes.h"
int memcmp(void *s1, void *s2, unsigned long n);

int main(void) {
    struct bytesize1 s1 = fun1();
    if (memcmp(&s1, &globvar_1, sizeof s1)) {
        return 1;
    }

    struct bytesize2 s2 = fun2();
    if (memcmp(&s2, &globvar_2, sizeof s2)) {
        return 2;
    }

    struct bytesize3 s3 = fun3();
    if (memcmp(&s3, &globvar_3, sizeof s3)) {
        return 3;
    }

    struct bytesize4 s4 = fun4();
    if (memcmp(&s4, &globvar_4, sizeof s4)) {
        return 4;
    }

    struct bytesize5 s5 = fun5();
    if (memcmp(&s5, &globvar_5, sizeof s5)) {
        return 5;
    }

    struct bytesize6 s6 = fun6();
    if (memcmp(&s6, &globvar_6, sizeof s6)) {
        return 6;
    }

    struct bytesize7 s7 = fun7();
    if (memcmp(&s7, &globvar_7, sizeof s7)) {
        return 7;
    }

    struct bytesize8 s8 = fun8();
    if (memcmp(&s8, &globvar_8, sizeof s8)) {
        return 8;
    }

    struct bytesize9 s9 = fun9();
    if (memcmp(&s9, &globvar_9, sizeof s9)) {
        return 9;
    }

    struct bytesize10 s10 = fun10();
    if (memcmp(&s10, &globvar_10, sizeof s10)) {
        return 10;
    }

    struct bytesize11 s11 = fun11();
    if (memcmp(&s11, &globvar_11, sizeof s11)) {
        return 11;
    }

    struct bytesize12 s12 = fun12();
    if (memcmp(&s12, &globvar_12, sizeof s12)) {
        return 12;
    }

    struct bytesize13 s13 = fun13();
    if (memcmp(&s13, &globvar_13, sizeof s13)) {
        return 13;
    }

    struct bytesize14 s14 = fun14();
    if (memcmp(&s14, &globvar_14, sizeof s14)) {
        return 14;
    }

    struct bytesize15 s15 = fun15();
    if (memcmp(&s15, &globvar_15, sizeof s15)) {
        return 15;
    }

    struct bytesize16 s16 = fun16();
    if (memcmp(&s16, &globvar_16, sizeof s16)) {
        return 16;
    }

    struct bytesize17 s17 = fun17();
    if (memcmp(&s17, &globvar_17, sizeof s17)) {
        return 17;
    }

    struct bytesize18 s18 = fun18();
    if (memcmp(&s18, &globvar_18, sizeof s18)) {
        return 18;
    }

    struct bytesize19 s19 = fun19();
    if (memcmp(&s19, &globvar_19, sizeof s19)) {
        return 19;
    }

    struct bytesize20 s20 = fun20();
    if (memcmp(&s20, &globvar_20, sizeof s20)) {
        return 20;
    }

    struct bytesize21 s21 = fun21();
    if (memcmp(&s21, &globvar_21, sizeof s21)) {
        return 21;
    }

    struct bytesize22 s22 = fun22();
    if (memcmp(&s22, &globvar_22, sizeof s22)) {
        return 22;
    }

    struct bytesize23 s23 = fun23();
    if (memcmp(&s23, &globvar_23, sizeof s23)) {
        return 23;
    }

    struct bytesize24 s24 = fun24();
    if (memcmp(&s24, &globvar_24, sizeof s24)) {
        return 24;
    }

    return 0;
}

struct bytesize1 globvar_1 = {{0}};

struct bytesize2 globvar_2 = {{1, 2}};

struct bytesize3 globvar_3 = {{3, 4, 5}};

struct bytesize4 globvar_4 = {{6, 7, 8, 9}};

struct bytesize5 globvar_5 = {{10, 11, 12, 13, 14}};

struct bytesize6 globvar_6 = {{15, 16, 17, 18, 19, 20}};

struct bytesize7 globvar_7 = {{21, 22, 23, 24, 25, 26, 27}};

struct bytesize8 globvar_8 = {{28, 29, 30, 31, 32, 33, 34, 35}};

struct bytesize9 globvar_9 = {{36, 37, 38, 39, 40, 41, 42, 43, 44}};

struct bytesize10 globvar_10 = {{45, 46, 47, 48, 49, 50, 51, 52, 53, 54}};

struct bytesize11 globvar_11 = {{55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65}};

struct bytesize12 globvar_12 = {
    {66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77}};

struct bytesize13 globvar_13 = {
    {78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90}};

struct bytesize14 globvar_14 = {
    {91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104}};

struct bytesize15 globvar_15 = {{105, 106, 107, 108, 109, 110, 111, 112, 113,
                                 114, 115, 116, 117, 118, 119}};

struct bytesize16 globvar_16 = {{120, 121, 122, 123, 124, 125, 126, 127, 128,
                                 129, 130, 131, 132, 133, 134, 135}};

struct bytesize17 globvar_17 = {{136, 137, 138, 139, 140, 141, 142, 143, 144,
                                 145, 146, 147, 148, 149, 150, 151, 152}};

struct bytesize18 globvar_18 = {{153, 154, 155, 156, 157, 158, 159, 160, 161,
                                 162, 163, 164, 165, 166, 167, 168, 169, 170}};

struct bytesize19 globvar_19 = {{171, 172, 173, 174, 175, 176, 177, 178, 179,
                                 180, 181, 182, 183, 184, 185, 186, 187, 188,
                                 189}};

struct bytesize20 globvar_20 = {{190, 191, 192, 193, 194, 195, 196,
                                 197, 198, 199, 200, 201, 202, 203,
                                 204, 205, 206, 207, 208, 209}};

struct bytesize21 globvar_21 = {{210, 211, 212, 213, 214, 215, 216,
                                 217, 218, 219, 220, 221, 222, 223,
                                 224, 225, 226, 227, 228, 229, 230}};

struct bytesize22 globvar_22 = {{231, 232, 233, 234, 235, 236, 237, 238,
                                 239, 240, 241, 242, 243, 244, 245, 246,
                                 247, 248, 249, 250, 251, 252}};

struct bytesize23 globvar_23 = {{253, 254, 255, 0,  1,  2,  3,  4,
                                 5,   6,   7,   8,  9,  10, 11, 12,
                                 13,  14,  15,  16, 17, 18, 19}};

struct bytesize24 globvar_24 = {{20, 21, 22, 23, 24, 25, 26, 27,
                                 28, 29, 30, 31, 32, 33, 34, 35,
                                 36, 37, 38, 39, 40, 41, 42, 43}};
