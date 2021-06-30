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

TEST_F(Robustness, check_stop_alignment){

    // when ::stop() has validation against the name, this should attempt a start() and stop() with mismatched names
    // and expect throw

}