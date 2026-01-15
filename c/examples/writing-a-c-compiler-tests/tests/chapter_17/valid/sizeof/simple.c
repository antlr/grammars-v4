/* Basic test of two forms of sizeof: referring to type names and expressions */

int main(void) {
    if (sizeof (int) != 4) {
        return 1;
    }

    if (sizeof 3.0 != 8) {
        return 2;
    }

    return 0;
}