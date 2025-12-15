/* If a switch body contains no case or default statements, we'll eliminate the whole thing */

int target(int flag) {
    switch (flag) {
        // Eliminate all of this - it's unreachable b/c outer
        // switch statement has no case/default statements (even
        // though inner switch does)
        static int x = 0;
        for (int i = 0; i < flag; i = i + 1) {
            switch (i) {
            case 1: x = x + 1;
            case 2: x = x + 2;
            default: x = x * 3;
            }
        }
        return x;
    }

    return 0;
}

int main(void) {
    return target(20);
}