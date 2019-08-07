
#pragma once

class TimeScoper {
    std::chrono::microseconds& acc;
    std::chrono::high_resolution_clock::time_point t1;
public:
    
    TimeScoper(std::chrono::microseconds& acc) : acc(acc), t1(std::chrono::high_resolution_clock::now()) {}
    
    ~TimeScoper() {
        auto t2 = std::chrono::high_resolution_clock::now();
        
        std::chrono::microseconds span = std::chrono::duration_cast<std::chrono::microseconds>(t2 - t1);
        
        acc += span;
    }
};

