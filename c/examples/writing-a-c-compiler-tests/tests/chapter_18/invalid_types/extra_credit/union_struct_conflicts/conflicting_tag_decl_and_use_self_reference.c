/* It's illegal to use a 'union s' type when a 'struct s' type is in scope ,
 * or vice versa.
 */

int main(void) {
    struct s;
    {
        union s* ptr;
    }
    return 0;
}