/* Test for meet operator: if different copies to a variable
 * reach the ends of a block's predecessors, no copies to that variable
 * reach that block
 * */

int multi_path(int flag) {
    int x = 3;  // generate x = 3
    if (flag)
        x = 4;  // kill x = 3, generate x = 4

    // One predecessor of our final block has outgoing copy x = 3,
    // the other has outgoing copy x = 4. Their intersection is the empty set,
    // so we can't propagate any value to this return statement.
    return x;
}

int main(void) {
    if (multi_path(1) != 4) {
        return 1;
    }

    if (multi_path(0) != 3) {
        return 2;
    }

    return 0;  // success
}