// Test that we can use coalescing to prevent spills. In particular,
// this tests that we rebuild the interference graph after each coalescing
// loop, allowing us to find new coalescing opportunities and ultimately
// color an up-to-date graph.
int glob = 5;
int flag = 0;

// defined in tests/chapter_20/helper_libs/coalesce_prevents_spill_lib.c
int validate(int a, int b, int c, int d, int e, int f, int g, int h, int i,
             int j, int k, int l, int m);

int target(int arg) {
    // declare some pseudoregisters but don't initialize them yet
    int a;
    int b;
    int c;
    int d;
    int e;
    int f;
    int g;
    int h;
    int i;
    int j;
    int k;
    int l;
    int m;
    // a-m all appear to conflict, which would require us to spill one of them,
    // but they actually have the same value.
    if (flag) {
        // This branch isn't taken; only included to prevent copy propagation.
        // It creates conflict between a-m, which will go away as we perform coalescing.
        // the same value
        a = arg;
        b = arg;
        c = arg;
        d = arg;
        e = arg;
        f = arg;
        g = arg;
        h = arg;
        i = arg;
        j = arg;
        k = arg;
        l = arg;
        m = arg;
    } else {
        // This branch creates conflicts between a-m, which will go away as we perform coalescing.
        a = glob * 2;  // 10
        b = a;
        c = a;
        d = a;
        e = a;
        f = a;
        g = a;
        h = a;
        i = a;
        j = a;
        k = a;
        l = a;
        m = a;
    }
    // We'll first coalesce a-f into param-passing registers.
    // After rebuilding interference graph, we'll recognize that g-m don't
    // conflict with these registers despite originally conflicting with a-f,
    // so we'll be able to coalesce them into DI.
    return validate(a, b, c, d, e, f, g, h, i, j, k, l, m);
}