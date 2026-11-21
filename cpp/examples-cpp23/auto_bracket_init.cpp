// C++23 allows auto x { expr } or auto(x) for functional-style casts.
// Specifically, initializing auto from a braced-init-list with a single element as decay-copy.
#include <iostream>

int main() {
    int x = 10;
    auto y = auto(x); // C++23 decay copy cast notation
    std::cout << y << "\n";
    return 0;
}
