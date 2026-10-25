// SignedUnsignedIntegrals.cpp

#include <iostream>

template <typename T>
concept Integral = std::is_integral<T>::value;

template <typename T>
concept SignedIntegral = Integral<T> && std::is_signed<T>::value;

template <typename T>
concept UnsignedIntegral = Integral<T> && !SignedIntegral<T>;

void func(SignedIntegral auto integ) {
    std::cout << "SignedIntegral: " << integ << '\n';
}

void func(UnsignedIntegral auto integ) {
    std::cout << "UnsignedIntegral: " << integ << '\n';
}

int main() {

    std::cout << '\n';

    func(-5);
    func(5u);

    std::cout << '\n';

}

