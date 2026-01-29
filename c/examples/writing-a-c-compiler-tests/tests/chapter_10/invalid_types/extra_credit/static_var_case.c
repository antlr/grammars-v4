// Can't use a static variable as a case in a switch statement

int main(void) {
    static int i = 0;

    switch(0) {
        case i: return 0;
    }
    return 0;
}