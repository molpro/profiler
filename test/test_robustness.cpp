#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include <molpro/profiler/Counter.h>
#include <molpro/profiler/Node.h>
#include <molpro/profiler/Profiler.h>
#include <chrono>
#include <thread>

struct Robustness : ::testing::Test {

    // to be populated when/if more tests are added

};

int dummy_function(std::chrono::milliseconds time){
    std::this_thread::sleep_for(time);
    return time.count();
}

TEST_F(Robustness, check_premature_stop){
    int time = 0;
    auto prof = molpro::profiler::Profiler::single("MainProfiler");
    std::cout << prof->get_current_depth() << "\n";
    prof->start("check_premature_stop");
    std::cout << prof->get_current_depth() << "\n";
    prof->stop();
    std::cout << prof->get_current_depth() << "\n";
    prof->validate();
}

TEST_F(Robustness, check_late_stop){
    int time = 0;
    auto prof = molpro::profiler::Profiler::single("MainProfiler");
    prof->start("scope1");
    {
        prof->start("check_late_stop");
        time += dummy_function(std::chrono::milliseconds(500));
        prof->stop();
    }
    time += dummy_function(std::chrono::milliseconds(500));

    auto recorded_time = prof->root->counter.get_wall().cumulative_time()*1000;
    std::cout << "Time       :" << recorded_time << "\n";
    std::cout << "Actual time:" << time << "\n";
    std::cout << "cpstime    :" <<
    prof->root->child("scope1")->counter.get_wall().cumulative_time()*1000 << "\n";

    std::cout << "dummy_time :" <<
    prof->root->child("scope1")->child("check_late_stop")->counter.get_wall().cumulative_time()*1000
     << "\n";

    time += dummy_function(std::chrono::milliseconds(500));
    auto recorded_time2 = prof->root->counter.get_wall().cumulative_time()*1000;
    std::cout << "Actual time:" << recorded_time2 << "\n"; //todo: use expect throw

}