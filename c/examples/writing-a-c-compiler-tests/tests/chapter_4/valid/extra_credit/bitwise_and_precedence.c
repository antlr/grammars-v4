#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wparentheses"
#endif

int main(void) {
    // & has lower precedence than ==
    return 5 & 7 == 5;
}