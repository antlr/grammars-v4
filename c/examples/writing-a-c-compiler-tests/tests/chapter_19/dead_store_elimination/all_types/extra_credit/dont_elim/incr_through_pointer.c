/* Dead store elimination should never eliminate Store instructions
 * */
void f(int *ptr) {
    ++*ptr;  // not a dead store!
    return;
}

int main(void) {
    int x = 0;
    f(&x);
    return x;
}