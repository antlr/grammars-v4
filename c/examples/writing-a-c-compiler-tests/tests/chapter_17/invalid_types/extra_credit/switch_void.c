// Can't use void controlling expression in switch statement
void f(void) {
    return;
}

int main(void) {
    switch(f()) {
        default: return 0;
    }
}