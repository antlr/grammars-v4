// C++23 introduced 'if consteval'. C++20 only supports 'std::is_constant_evaluated()'.
// This will cause a syntax error in C++20.
#include <iostream>

constexpr int compute() {
    if consteval {
        return 42; // Executed during compile time
    } else {
        return 0;  // Executed during runtime
    }
}

int main() {
    constexpr int val = compute();
    std::cout << val << "\n";
    return 0;
}
