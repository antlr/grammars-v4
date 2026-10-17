int main(void) {
    int x = 10;
    int y = 0;
    int z = 0;
    do {
        z = z + 1;
        if (x <= 0)
            continue;
        x = x - 1;
        if (y >= 10)
            continue;
        y = y + 1;
    } while (z != 50);
    return z == 50 && x == 0 && y == 10;
}
