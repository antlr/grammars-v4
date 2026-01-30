/* Test that we can pass static and automatic structs of every size between 1 and 24 bytes.
 * Pass each size both in a register (when possible) and on the stack. */
#include "struct_sizes.h"

int main(void) {

    // pass global variables of each size
    if (!fun0(globvar_1, globvar_2, globvar_3, globvar_4, globvar_5, globvar_6,
             globvar_7, globvar_8, globvar_9, globvar_10, globvar_11,
             globvar_12, globvar_13, globvar_14, globvar_15, globvar_16,
             globvar_17, globvar_18, globvar_19, globvar_20, globvar_21,
             globvar_22, globvar_23, globvar_24, globvar_1.arr, globvar_2.arr,
             globvar_3.arr, globvar_4.arr, globvar_5.arr, globvar_6.arr,
             globvar_7.arr, globvar_8.arr, globvar_9.arr, globvar_10.arr,
             globvar_11.arr, globvar_12.arr, globvar_13.arr, globvar_14.arr,
             globvar_15.arr, globvar_16.arr, globvar_17.arr, globvar_18.arr,
             globvar_19.arr, globvar_20.arr, globvar_21.arr, globvar_22.arr,
             globvar_23.arr, globvar_24.arr)) {
        return 1;
    }

    if (!fun1(globvar_7, globvar_8, globvar_9, globvar_10, globvar_1, globvar_2,
             globvar_3, globvar_4, globvar_5, globvar_6, globvar_7.arr,
             globvar_8.arr, globvar_9.arr, globvar_10.arr, globvar_1.arr,
             globvar_2.arr, globvar_3.arr, globvar_4.arr, globvar_5.arr,
             globvar_6.arr)) {
        return 2;
    }

    if (!fun2(globvar_11, globvar_12, globvar_13, globvar_1, globvar_11.arr,
             globvar_12.arr, globvar_13.arr, globvar_1.arr)) {
        return 3;
    }

    if (!fun3(globvar_14, globvar_15, globvar_16, globvar_2, globvar_14.arr,
             globvar_15.arr, globvar_16.arr, globvar_2.arr)) {
        return 4;
    }

    // define local variables of each size
    struct bytesize1 locvar_1 = {{0}};

    struct bytesize2 locvar_2 = {{1, 2}};

    struct bytesize3 locvar_3 = {{3, 4, 5}};

    struct bytesize4 locvar_4 = {{6, 7, 8, 9}};

    struct bytesize5 locvar_5 = {{10, 11, 12, 13, 14}};

    struct bytesize6 locvar_6 = {{15, 16, 17, 18, 19, 20}};

    struct bytesize7 locvar_7 = {{21, 22, 23, 24, 25, 26, 27}};

    struct bytesize8 locvar_8 = {{28, 29, 30, 31, 32, 33, 34, 35}};

    struct bytesize9 locvar_9 = {{36, 37, 38, 39, 40, 41, 42, 43, 44}};

    struct bytesize10 locvar_10 = {{45, 46, 47, 48, 49, 50, 51, 52, 53, 54}};

    struct bytesize11 locvar_11 = {
        {55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65}};

    struct bytesize12 locvar_12 = {
        {66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77}};

    struct bytesize13 locvar_13 = {
        {78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90}};

    struct bytesize14 locvar_14 = {
        {91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104}};

    struct bytesize15 locvar_15 = {{105, 106, 107, 108, 109, 110, 111, 112, 113,
                                    114, 115, 116, 117, 118, 119}};

    struct bytesize16 locvar_16 = {{120, 121, 122, 123, 124, 125, 126, 127, 128,
                                    129, 130, 131, 132, 133, 134, 135}};

    struct bytesize17 locvar_17 = {{136, 137, 138, 139, 140, 141, 142, 143, 144,
                                    145, 146, 147, 148, 149, 150, 151, 152}};

    struct bytesize18 locvar_18 = {{153, 154, 155, 156, 157, 158, 159, 160, 161,
                                    162, 163, 164, 165, 166, 167, 168, 169,
                                    170}};

    struct bytesize19 locvar_19 = {{171, 172, 173, 174, 175, 176, 177, 178, 179,
                                    180, 181, 182, 183, 184, 185, 186, 187, 188,
                                    189}};

    struct bytesize20 locvar_20 = {{190, 191, 192, 193, 194, 195, 196,
                                    197, 198, 199, 200, 201, 202, 203,
                                    204, 205, 206, 207, 208, 209}};

    struct bytesize21 locvar_21 = {{210, 211, 212, 213, 214, 215, 216,
                                    217, 218, 219, 220, 221, 222, 223,
                                    224, 225, 226, 227, 228, 229, 230}};

    struct bytesize22 locvar_22 = {{231, 232, 233, 234, 235, 236, 237, 238,
                                    239, 240, 241, 242, 243, 244, 245, 246,
                                    247, 248, 249, 250, 251, 252}};

    struct bytesize23 locvar_23 = {{253, 254, 255, 0,  1,  2,  3,  4,
                                    5,   6,   7,   8,  9,  10, 11, 12,
                                    13,  14,  15,  16, 17, 18, 19}};

    struct bytesize24 locvar_24 = {{20, 21, 22, 23, 24, 25, 26, 27,
                                    28, 29, 30, 31, 32, 33, 34, 35,
                                    36, 37, 38, 39, 40, 41, 42, 43}};

    // pass local variables of each size
    if (!fun0(locvar_1, locvar_2, locvar_3, locvar_4, locvar_5, locvar_6,
             locvar_7, locvar_8, locvar_9, locvar_10, locvar_11, locvar_12,
             locvar_13, locvar_14, locvar_15, locvar_16, locvar_17, locvar_18,
             locvar_19, locvar_20, locvar_21, locvar_22, locvar_23, locvar_24,
             locvar_1.arr, locvar_2.arr, locvar_3.arr, locvar_4.arr,
             locvar_5.arr, locvar_6.arr, locvar_7.arr, locvar_8.arr,
             locvar_9.arr, locvar_10.arr, locvar_11.arr, locvar_12.arr,
             locvar_13.arr, locvar_14.arr, locvar_15.arr, locvar_16.arr,
             locvar_17.arr, locvar_18.arr, locvar_19.arr, locvar_20.arr,
             locvar_21.arr, locvar_22.arr, locvar_23.arr, locvar_24.arr)) {
        return 5;
    }

    if (!fun1(locvar_7, locvar_8, locvar_9, locvar_10, locvar_1, locvar_2,
             locvar_3, locvar_4, locvar_5, locvar_6, locvar_7.arr, locvar_8.arr,
             locvar_9.arr, locvar_10.arr, locvar_1.arr, locvar_2.arr,
             locvar_3.arr, locvar_4.arr, locvar_5.arr, locvar_6.arr)) {
        return 6;
    }

    if (!fun2(locvar_11, locvar_12, locvar_13, locvar_1, locvar_11.arr,
             locvar_12.arr, locvar_13.arr, locvar_1.arr)) {
        return 7;
    }

    if (!fun3(locvar_14, locvar_15, locvar_16, locvar_2, locvar_14.arr,
             locvar_15.arr, locvar_16.arr, locvar_2.arr)) {
        return 8;
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
