/* A ++/-- operation kills its operand */

int target(int flag) {
    int w = 3;
    if (flag) {
        w++;
    }

    int x = 10;
    if (flag) {
        x--;
    }

    int y = -12;
    if (flag) {
        ++y;
    }

    int z = -100;
    if (flag) {
        --z;
    }

    if (flag) {
        if (w == 4 && x == 9 && y == -11 && z == -101) {
            // success
            return 0;
        }
        return 1;
    }
    else {
        if (w == 3 && x == 10 && y == -12 && z == -100) {
            // success
            return 0;
        }
        return 1; // fail

    }

}

int main(void) {
    if (target(0)) {
        return 1; // fail
    }

    if (target(1)) {
        return 2; // fail
    }

    return 0;
}
