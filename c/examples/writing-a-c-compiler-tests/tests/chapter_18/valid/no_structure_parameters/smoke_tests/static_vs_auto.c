// make sure structs w/ automatic storage duration are reinitialized whenver
// they come into scope and structs w/ static storage duration are initialized
// only once

struct s {
    int a;
    int b;
};

int main(void) {
    for (int i = 0; i < 10; i = i + 1) {
        struct s autom = {1, 2};
        static struct s stat = {1, 2};

        autom.a = autom.a + 1;
        autom.b = autom.b + 1;

        stat.a = stat.a + 1;
        stat.b = stat.b + 1;

        // on last iteration, validate both structs
        if (i == 9) {
            // stat should be {10, 11} b/c both members were incremented on each
            // iteration
            if (stat.a != 11 || stat.b != 12) {
                return 1;
            }
            // autom should be {2, 3} b/c it was reinitialized on every
            // iteration
            if (autom.a != 2 || autom.b != 3) {
                return 2;
            }
        }
    }

    return 0;  // success
}