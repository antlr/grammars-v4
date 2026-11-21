// nestedRequirements.cpp

#include <type_traits>

template <typename T>
concept Integral = std::is_integral<T>::value;

template <typename T>
concept SignedIntegral = Integral<T> && std::is_signed<T>::value;

// template <typename T>
// concept UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

template <typename T>
concept UnsignedIntegral = Integral<T> &&
requires(T) {
    requires !SignedIntegral<T>;
};

int main() {

    UnsignedIntegral auto n = 5u;  // works
    // UnsignedIntegral auto m = 5;   // compile time error, 5 is a signed literal

}