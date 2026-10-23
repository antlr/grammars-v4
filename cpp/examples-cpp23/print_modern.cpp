// C++23 feature: std::print
// This will not compile in C++20 because <print> and std::print do not exist.
#include <print>
#include <string>

int main() {
    std::string name = "C++23";
    // В C++23 форматированный вывод теперь встроен прямо в стандарт печати
    std::print("Hello, {}! Welcome to the future.\n", name);
    std::println("This is standard {} printing.", 2023);
    return 0;
}
