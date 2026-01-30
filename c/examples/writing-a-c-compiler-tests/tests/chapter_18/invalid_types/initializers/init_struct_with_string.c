struct chars {
    char a;
    char b;
    char c;
    char null;
};

int main(void) {

    // you can't initialize structure members with a string,
    // even if they're all chars
    struct chars my_chars = "abc";
    return 0;
}