int a(void) {
    return 1;
}

int b(int a) {
    return a;
}

int main(void) {
    return a() + b(2);
}