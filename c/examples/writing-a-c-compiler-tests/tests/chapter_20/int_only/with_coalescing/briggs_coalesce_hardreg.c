/* Test that the Briggs test also applies to hard registers.
 * In other words, we'll coalesce a pseudoregister and hard register that pass
 * the Briggs test even if they don't pass the George test. We inspect the
 * assembly for the target function to validate that it contains no spills and
 * no mov instructions whose source and destination are both general-purpose
 * registers (except mov %rsp, %rbp and mov %rbp, %rsp in the prologue and
 * epilogue).
 * NOTE: it's theoretically possible but very unlikely that every variable
 * could end up in the right register by accident and the test could pass
 * even if we don't perform register coalescing.
 * */

#include "../util.h"

int flag = 1;

int target(void) {
    int coalesce_into_eax = 0;
    // put initializer in condition to prevent copy prop
    if (flag) {  // always taken
        // We move the function's return value from EAX into this variable;
        // we can coalesce these because they pass the Briggs test.
        coalesce_into_eax = id(10);
    }

    // Define a variable with significant degree that interferes with
    // coalesce_into_eax, and with the arguments passed in param-passing
    // registers below, but does not interfere with any hard registers.
    int high_degree = 2 * coalesce_into_eax;  // 20

    // Validate coalesce_into_eax. NOTE: don't use check_one_int here to avoid
    // coalescing this into EDI or create a mov instruction between registers
    if (coalesce_into_eax != 10) {
        return -1;
    }

    // Make high_degree interfere with twelve other variables so it has
    // significant degree. Five of these variables will be passed as arguments;
    // we'll use the Briggs test to coalesce them into the parameter-passing
    // registers. They won't pass the George test because they interfere with
    // high_degree. NOTE: We use constants as first operand of each intializing
    // expression so we don't move pseudos from one register to another, and we
    // use each of these variables at least once so it isn't removed by DSE.
    int twelve = 32 - high_degree;
    int eleven = 23 - twelve;
    int ten = 21 - eleven;
    int nine = 19 - ten;
    int eight = 17 - nine;
    int seven = 15 - eight;
    int six = 13 - seven;
    // Define the variables passed as arguments below; use high_degree
    // so that it interferes with all of them
    int five = 11 - six;
    int four = 24 - high_degree;
    int three = 23 - high_degree;
    int two = 22 - high_degree;
    int one = 21 - high_degree;
    // Validate one-five
    // NOTE: we can't check all 12 with check_12_ints, or with multiple check_*
    // calls, because that would make more pseudos interfere with EAX
    check_5_ints(one, two, three, four, five, 1);

    return 0;  // success
}