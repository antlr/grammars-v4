int main(void) {
    int sum = 0;
    int counter;
    for (int i = 0; i <= 10; i = i + 1) {
        counter = i;
        if (i % 2 == 0)
            continue;
        sum = sum + 1;
    }

    return sum == 5 && counter == 10;
}
