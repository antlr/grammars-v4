int main(void) {
    int i = 100;
    int sum = 0;
    do sum += 2;
    while (i -= 1);
    return (i == 0 && sum == 200);
}