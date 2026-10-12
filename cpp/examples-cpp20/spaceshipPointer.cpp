// spaceshipPoiner.cpp

#include <iostream>
#include <compare>
#include <vector>

struct A {
    std::vector<int>* pointerToVector;
    auto operator <=> (const A&) const = default;
};

int main() {

    std::cout << '\n';

    std::cout << std::boolalpha;

    A a1{new std::vector<int>()};
    A a2{new std::vector<int>()};

    std::cout << "(a1 == a2): " << (a1 == a2) << "\n\n";

}