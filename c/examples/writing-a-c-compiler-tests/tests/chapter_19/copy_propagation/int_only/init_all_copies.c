/* Test that we initialize each basic block with the set of all copies
 * in the function
 * */

int counter = 0;

int increment_counter(void) {
    counter = counter + 1;
    return 0;
}

int target(void) {
    int y = 3;
    do {
        // when we first process this block,
        // y = 3 will reach it from one predecessor, and we won't have
        // visited the other yet; make sure we still recognize
        // that y = 3 reaches this block (and its successor)
        increment_counter();
    } while (counter < 5);
    return y;  // this should become return 3
}

int main(void) {
    int result = target();
    if (result != 3) {
        return 1;
    }

    // make sure we looped the right number of times
    if (counter != 5) {
        return 2;
    }

    return 0;  // success
}