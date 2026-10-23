// C++23 feature: std::expected
// This will not compile in C++20 because <expected> and std::expected do not exist.
#include <iostream>
#include <expected>
#include <string>

std::expected<int, std::string> divide(int a, int b) {
    if (b == 0) {
        return std::unexpected("Division by zero error");
    }
    return a / b;
}

int main() {
    auto res = divide(10, 0);
    if (!res) {
        std::cout << "Error: " << res.error() << std::endl;
    } else {
        std::cout << "Result: " << *res << std::endl;
    }
    return 0;
}
