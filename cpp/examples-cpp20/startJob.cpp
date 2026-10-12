// startJob.cpp

#include <coroutine>
#include <iostream>
 
struct Job { 
    struct promise_type;
    using handle_type = std::coroutine_handle<promise_type>;
    handle_type coro;
    Job(handle_type h): coro(h){}
    ~Job() {
        if ( coro ) coro.destroy();
    }
    void start() {
        coro.resume();
    }


    struct promise_type {
        auto get_return_object() { 
            return Job{handle_type::from_promise(*this)};
         }
        std::suspend_always initial_suspend() { 
            std::cout << "    Preparing job" << '\n';
            return {}; 
        }
        std::suspend_always final_suspend() noexcept { 
            std::cout << "    Performing job" << '\n'; 
            return {}; 
        }
        void return_void() {}
        void unhandled_exception() {}
    
    };
};
 
Job prepareJob() {
    co_await std::suspend_never();
}
 
int main() {

    std::cout <<  "Before job" << '\n';

    auto job = prepareJob();
    job.start();

    std::cout <<  "After job" <<  '\n';

}