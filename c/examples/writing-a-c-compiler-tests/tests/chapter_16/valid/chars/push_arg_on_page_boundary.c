#ifdef SUPPRESS_WARNINGS
#pragma GCC diagnostic ignored "-Wunused-parameter"
#endif

// use an int that's within 8 bytes of a page boundary as a stack argument
// this makes sure we don't use 8-byte push to push 4-byte values in memory
// NOTE: this doesn't test what it's supposed to on macOS, only on Linux;
// on Linux the BSS section is at the end of the executable, followed by unmapped memory
// on macOS the last section in the executable is __LINKEDIT so if we overrun
// a page boundary we don't hit unmapped memory

extern char zed; // defined in data_on_page_boundary.s
int foo(int a, int b, int c, int d, int e, int f, char g) {
    return g + 1;
}

int main(void) {
    return foo(0, 0, 0, 0, 0, 0, zed);
}