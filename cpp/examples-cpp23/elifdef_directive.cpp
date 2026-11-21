// C++23 feature: #elifdef and #elifndef
// This will not compile or will cause a preprocessor error in C++20 
// because these directives were introduced only in C++23.
#include <iostream>

#define MODE_B

int main() {
#ifdef MODE_A
    std::cout << "Mode A active\n";
#elifdef MODE_B
    // В C++20 препроцессор не знает директиву #elifdef и выдаст ошибку
    std::cout << "Mode B active via #elifdef\n";
#else
    std::cout << "Default mode\n";
#endif
    return 0;
}
