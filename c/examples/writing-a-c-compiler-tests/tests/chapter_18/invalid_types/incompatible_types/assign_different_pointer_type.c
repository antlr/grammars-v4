struct s1;
struct s2;

int main(void) {
    struct s1 *p1 = 0;
    struct s2 *p2 = 0;
    p2 = p1;  // can't assign to struct s2 * from struct s1 *
    return 0;
}