// C++23 introduced "Deducing this" (explicit object parameters).
// Specifying 'this' as the first parameter of a member function is a syntax error in C++20.
#include <iostream>

struct Separator {
    int value = 100;
    // C++23 explicit object parameter
    void print(this Separator const& self) {
        std::cout << self.value << "\n";
    }
};

int main() {
    Separator s;
    s.print();
    return 0;
}
