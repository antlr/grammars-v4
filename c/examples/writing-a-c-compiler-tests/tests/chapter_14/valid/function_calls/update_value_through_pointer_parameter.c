/* Make sure that a callee can update an object through a variable passed by the caller */
int update_value(int *ptr) {
    int old_val = *ptr;
    *ptr = 10;
    return old_val;
}

int main(void) {
    int x = 20;
    int result = update_value(&x);
    if (result != 20) {
        return 1;
    }
    if (x != 10) {
        return 2;
    }
    return 0;
}