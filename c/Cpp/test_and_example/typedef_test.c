/* Test typedef disambiguation — the core reason CParserBase exists. */

typedef unsigned long size_t;
typedef int BOOL;

struct Point {
    int x;
    int y;
};

typedef struct Point Point;

BOOL compare(Point a, Point b) {
    return a.x == b.x && a.y == b.y;
}

int main() {
    size_t n;
    n = 42;
    Point p;
    p.x = 1;
    p.y = 2;
    BOOL eq;
    eq = compare(p, p);
    return 0;
}
