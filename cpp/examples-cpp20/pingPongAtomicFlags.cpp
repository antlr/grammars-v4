// pingPongAtomicFlags.cpp

#include <iostream>
#include <atomic>
#include <thread>

std::atomic_flag condAtomicFlag1{};
std::atomic_flag condAtomicFlag2{};

std::atomic<int> counter{};
constexpr int countlimit = 1'000'000;

void ping() {
    while(counter <= countlimit) {
        condAtomicFlag1.wait(false);
        condAtomicFlag1.clear();

        ++counter;
        
        condAtomicFlag2.test_and_set();
        condAtomicFlag2.notify_one();
    }
}

void pong() {
    while(counter < countlimit) {
        condAtomicFlag2.wait(false);
        condAtomicFlag2.clear();
        
        condAtomicFlag1.test_and_set();
        condAtomicFlag1.notify_one();
    }
}

int main() {

    auto start = std::chrono::system_clock::now();  

    condAtomicFlag1.test_and_set();
    std::thread t1(ping);
    std::thread t2(pong);

    t1.join();
    t2.join();

    std::chrono::duration<double> dur = std::chrono::system_clock::now() - start;
    std::cout << "Duration: " << dur.count() << " seconds" << '\n';

}


