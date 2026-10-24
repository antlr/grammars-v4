/* If there are multiple paths from x = 3 to a use of x, but the copy
 * isn't killed on any of those paths, we can propagate it.
 * */
int var = 0;
int callee(void) {
    var = var + 1;
    return 0;
}

int target(int flag) {
    int x = 3;
    if (flag)
        callee();
    return x;  // should become return 3
}

int main(void) {
    if (target(0) != 3) {
        return 1;
    }
    if (target(1) != 3) {
        return 2;
    }
    // make sure callee was called once
    if (var != 1) {
        return 3;
    }
    return 0;  // success
}