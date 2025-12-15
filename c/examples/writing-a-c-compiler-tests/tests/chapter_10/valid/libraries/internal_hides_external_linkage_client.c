/* A variable with internal linkage will hide a variable with the same name
 * in a different file, even if the variable in the other file has external linkage.
 */

static int x = 1;

// read the value of this file's x variable
int read_internal_x(void);

// read the other file's x variable, which has external linkage
int read_x(void);

int main(void) {
    // This refers to the variable with internal linkage
    // defined above
    extern int x;
    if (x != 1)
        return 1;
    // update x, make sure its value is visible in another function
    x = 2;

    if (read_internal_x() != 2)
        return 1;

    // validate that other x was defined and initialized correctly
    if (read_x() != 10)
        return 1;
    return 0;
}

// this refers to the 'x' variable defines in this file with internal linkage
extern int x;

int read_internal_x(void) {
    return x;

}