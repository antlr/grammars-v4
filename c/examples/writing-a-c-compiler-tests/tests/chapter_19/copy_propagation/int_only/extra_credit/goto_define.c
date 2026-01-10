int target(int flag) {
    int x = 10;
    goto def_x;
    if (flag) {
    def_x:
        x = 20;
    }
    return x; // return 20
}

int main(void) {
    return target(0);
}