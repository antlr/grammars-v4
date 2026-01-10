#ifdef SUPPRESS_WARNINGS
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wempty-body"
#endif
#endif

int main(void) {
    int i = 2147483642;
    do ; while ((i = i - 5) >= 256);

    return i;
}
