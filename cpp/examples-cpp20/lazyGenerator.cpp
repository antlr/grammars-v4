// lazyGenerator.cpp

#include <iostream>

generator<int> generatorForNumbers(int begin, int inc = 1) {
  
    while (true) {
        co_yield i;
    }
  
}

int main() {

    std::cout << '\n';

    const auto numbers = generatorForNumbers(-10);
  
    for (int i= 1; i <= 20; ++i) std::cout << numbers() << " ";
  
    std::cout << "\n\n";

    for (auto n: generatorForNumbers(0, 5)) std::cout << n << " ";

    std::cout << "\n\n";

}