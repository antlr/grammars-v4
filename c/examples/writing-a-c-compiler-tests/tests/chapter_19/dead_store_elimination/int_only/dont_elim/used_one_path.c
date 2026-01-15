/* A variable is live if it's used later on one path but not others.
 * Loosely based on figure 19-10
 * */

int f(int arg, int flag) {
    int x = arg * 2;  // not dead, b/c x is live on one path
    if (flag)
        return x;
    return 0;
}

int main(void) {
    if (f(20, 1) != 40) {
        return 1;  // fail
    }
    if (f(3, 0) != 0) {
        return 2;  // fail
    }
    return 0;  // success
}