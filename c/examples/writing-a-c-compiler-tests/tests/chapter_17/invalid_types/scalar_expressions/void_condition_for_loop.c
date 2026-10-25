void foo(void) {
    return;
}

int main(void) {
    // void expressions are non-scalar, so they can't be used as controlling conditions
    for (int i = 0; foo(); )
        ;
    return 0;
}