/* This declares foo with internal linkage */
static int foo;

int main(void) {
    return foo;
}

/* This declares foo with external linkage,
 * which conflicts with the previous declaration
 */
int foo = 3;