// Can't have two default statements in same enclosing switch, even in different scopes
int main(void) {
    int a = 10;
    switch (a) {
        case 1:
        for (int i = 0; i < 10; i = i + 1) {
            continue;
            while(1)
            default:;
        }
        case 2:
        return 0;
        default:;
    }
    return 0;
}