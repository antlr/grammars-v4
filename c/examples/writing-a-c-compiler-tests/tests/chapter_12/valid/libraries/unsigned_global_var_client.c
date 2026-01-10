/* Make sure we can interact with unsigned variables in other translation units*/

/* Declarations of variable/functions defined in library */
extern unsigned int ui;
unsigned int return_uint(void);
int return_uint_as_signed(void);
long return_uint_as_long(void);

int main(void) {
    if (ui != 4294967200u)
        return 0;

    // should be converted to 2^32 - 1 on assignment
    ui = -1;

    /* Make sure that our update to ui is visible in the other translation unit,
     * and that we correctly track function return types
     */
    long result = (long) return_uint();
    if (result != 4294967295l)
        return 0;

    result = (long) return_uint_as_signed();
    if (result != -1l)
        return 0;

    result = return_uint_as_long();
    if (result != 4294967295l)
        return 0;

    return 1;
}
