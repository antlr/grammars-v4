struct s {
    int y;
};

struct s x;
// dot operator must be immediately followed by member name
// (can't parenthesize it)
int main(void) {
    return x.(y);
}
