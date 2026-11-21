// C++23 makes many <cmath> functions (like std::sin, std::cos) constexpr.
// In C++20, std::sin cannot be evaluated in a constexpr context.
#include <cmath>
#include <iostream>

int main() {
    constexpr double val = std::sin(0.0); // Error in C++20, valid in C++23
    std::cout << val << "\n";
    return 0;
}
