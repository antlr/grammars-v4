// make sure we can handle two labels with the same ID, in two functions with
// the same name, in different translation units

int f(void) {
    goto x;
    return 0;
x:
    return 1;
}

int f_caller(void); // declared in same_label_same_fun.c

int main(void) {
    if (f() != 1) {
        return 1;  // fail
    }
    if (f_caller() !=
        2) {       // call "f" with internal linkage in other translation unit
        return 2;  // fail
    }
    return 0;  // success
}