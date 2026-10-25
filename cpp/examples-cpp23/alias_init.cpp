// C++23 feature: alias-declaration in init-statements
// This will not compile in C++20 because alias-declarations (using ...) 
// were not allowed inside the init-statement of if/switch/for.
#include <iostream>
#include <vector>
#include <type_traits>

int main() {
    std::vector<int> vec = {1, 2, 3};

    // В C++23 можно объявлять 'using' прямо внутри условия 'if'
    if (using VecType = std::vector<int>; !vec.empty()) {
        VecType copy = vec;
        std::cout << "Vector copy size: " << copy.size() << std::endl;
    }
    
    return 0;
}
