/* Make sure our analysis recognizes which registers are used by each function
 * call - same idea as chapter_20/int_only/no_coalescing/track_arg_registers.c,
 * but with structs too. Liveness analysis should recognize that only XMM0-XMM1
 * and RDI, RSI, and RDX are live right before we call callee().
 * If we assume any other parameter-passing registers are also live,
 * we'll conclude they're live from the start of the function until the
 * function call (since they're never updated) and won't be able to allocate
 * them, resulting in spills.
 * Test script validates that only 15 instructions in target access memory:
 * 8 to populate structures, and 7 to transfer those structures into
 * param-passing registers/push them onto the stack
 * */

#include "../util.h"

struct s1 {
    // XMM0
    double d;
    // RDI
    char c;
    int i;
};

struct s2 {
    // RSI
    unsigned long ul;
    // XMM1
    double d;
};

// passed in memory
struct s3 {
    double d1;
    double d2;
    signed char s;
};

// helper functions defined in
// tests/chapter_20/helper_libs/mixed_type_arg_registers_lib.c;
// both of these functions print an error message and exit if
// arguments don't have the expected values
int callee(struct s1 a, struct s2 b, char c, struct s3 in_mem);
int check_some_args(int one, long two, unsigned int three, unsigned long four,
                    char five, unsigned char six, signed char seven);

// Global variables
int glob1;
long glob2;
unsigned int glob3;
unsigned long glob4;
char glob5;
unsigned char glob6;
signed char glob7;
long glob8;

double glob1_d;
double glob2_d;
double glob3_d;
double glob4_d;
double glob5_d;
double glob6_d;
double glob7_d;
double glob8_d;
double glob9_d;
double glob10_d;
double glob11_d;
double glob12_d;
double glob13_d;

// Note: we deliberately have target use the same param-passing registers as
// callee; if liveness incorrectly thought that some reg was used by callee and
// therefore live, it still wouldn't interfere with the parameter passed to
// target in that reg, so the error wouldn't necessarily force a spill. (I think
// having _fewer_ params in target than in callee would be be fine.)
int target(int one, int two, int three, double one_d, double two_d) {
    // Create a clique of 12 integer pseudoregs
    // we'll use eight through twelve in arguments to callee
    long four = two + 2;
    char five = three + two;
    int six = 12 - one - two - three;
    unsigned int seven = 13 - six;
    unsigned char eight = four * two;
    unsigned long nine = three * three;
    signed long ten = six + four;
    signed char eleven = six * two - one;
    int twelve = six * two;

    // Create clique of 14 double pseudos; we'll use eleven_d through
    // fourteen_d in arguments to callee
    double three_d = one_d + two_d;
    double four_d = three_d + one_d;
    double five_d = two_d + three_d;
    double six_d = three_d * two_d;
    double seven_d = 13. - six_d;
    double eight_d = four_d * two_d;
    double nine_d = three_d * three_d;
    double ten_d = five_d * two_d;
    double eleven_d = seven_d * two_d - three_d;
    double twelve_d = eight_d * four_d - 20.;
    double thirteen_d = (nine_d + ten_d) - six_d;
    double fourteen_d = eleven_d + 3;

    // To make all our pseudoregs interfere with each other, without forcing
    // them to be callee-saved, copy all of them to global variables.
    // (We don't need to copy the ones that get passed to callee).

    // integers
    glob1 = one;
    glob2 = two;
    glob3 = three;
    glob4 = four;
    glob5 = five;
    glob6 = six;
    glob7 = seven;

    // doubles
    glob1_d = one_d;
    glob2_d = two_d;
    glob3_d = three_d;
    glob4_d = four_d;
    glob5_d = five_d;
    glob6_d = six_d;
    glob7_d = seven_d;
    glob8_d = eight_d;
    glob9_d = nine_d;
    glob10_d = ten_d;

    // now populate some structs that use our pseudoregs
    // and pass them to callee
    struct s1 arg1 = {eleven_d, eight, nine};
    struct s2 arg2 = {ten, twelve_d};
    struct s3 in_mem = {thirteen_d, fourteen_d, eleven};
    callee(arg1, arg2, twelve, in_mem);

    // validate globals
    check_some_args(glob1, glob2, glob3, glob4, glob5, glob6, glob7);
    check_14_doubles(glob1_d, glob2_d, glob3_d, glob4_d, glob5_d, glob6_d,
                     glob7_d, glob8_d, glob9_d, glob10_d, 11.0, 12.0, 13., 14.,
                     1);

    return 0;  // success
}