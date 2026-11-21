// C++23 introduces new standard floating-point types and suffixes like float16, float32, etc.
// The suffix 'f32' or 'bf16' is invalid in C++20.
#include <iostream>

int main() {
    auto f = 3.14f32; // C++23 fixed-width floating-point suffix
    std::cout << f << "\n";
    return 0;
}
