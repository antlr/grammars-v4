/* Test that we remove useless labels */
int target(void) {
    // This empty do loop will start with several labels that we don't jump to;
    // make sure they're removed
    do {
    } while (0);

    return 99;
}

int main(void) {
    return target();
}