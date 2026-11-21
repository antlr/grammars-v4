#include <algorithm>
#include <iostream>
#include <stacktrace>

int main()
{
    auto trace       = std::stacktrace::current();
    auto empty_trace = std::stacktrace{};

    // Print stacktrace
    std::for_each(trace.begin(), trace.end(),
                  [](const auto& f) { std::cout << f << '\n'; });

    if (empty_trace.begin() == empty_trace.end())
        std::cout << "stacktrace 'empty_trace' is indeed empty.\n";
}
