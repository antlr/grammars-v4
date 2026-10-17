int main(void) {
    int i = 0;
    while (1) {
        i = i + 1;
        if (i > 10)
            break;
    }
    int j = 10;
    while (1) {
        j = j - 1;
        if (j < 0)
            break;
    }
    int result = j == -1 && i == 11;
    return result;
}
