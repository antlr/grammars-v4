/* Test ternary expressions where both sides are void */

int i = 4;
int j = 5;
int flag_1 = 1;
int flag_0 = 0;
void incr_i(void) {
    i = i + 1;
}
void incr_j(void) {
    j = j + 1;
}
int main(void) {
    flag_1 ? incr_i() : incr_j();  // increment i
    flag_0 ? incr_i() : incr_j();  // increment j
    if (i != 5) {
        return 1;
    }
    if (j != 6) {
        return 2;
    }

    // try a nested void expression

    flag_0 ? incr_j() : flag_1 ? incr_i() : incr_j();

    if (i != 6) {
        return 3;
    }

    if (j != 6) {
        return 4;
    }

    return 0;
}