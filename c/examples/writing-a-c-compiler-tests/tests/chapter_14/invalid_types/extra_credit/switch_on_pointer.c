/* The controlling expression of a switch statement
 * must be an integer, not a pointer
 */
int main(void) {
    int *x = 0;
    switch(x) {
        case 0: return 0;
        default: return 1;
    }
}