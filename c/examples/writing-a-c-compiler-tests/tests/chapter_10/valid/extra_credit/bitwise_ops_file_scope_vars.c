// Test that we can perform bitwise operations on file-scope variables

int x = 1;
int y = 0;

int main(void) {
    y = -1;
    x = (x << 1) | 1; // x = 3
    if (x != 3) {
        return 1;
    }
    y = ((y & -5) ^ 12) >> 2; // y = -3
    if (y != -3) {
        return 2;
    }
    return 0;
}