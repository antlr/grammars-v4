int main(void)
{
    int *x = 0;
    // It's illegal to compare a pointer with an integer, including 0.
    // Note that 0 isn't implicitly converted to a null pointer constant here;
    // it's only implicitly converted when used in == and != operations.
    // Note #2: GCC/Clang allow this as a language extension,
    // and don't warn about it by default
    return x > 0;
}