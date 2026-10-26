int main(void) {
    int i = 0;
    int j = 0;
    int k = 1;
    for (int i = 100; i > 0; i = i - 1) {
        int i = 1;
        int j = i + k;
        k = j;
    }

    return k == 101 && i == 0 && j == 0;
}
