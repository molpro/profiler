set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG "v1.16.0"
)
FetchContent_MakeAvailable(googletest)
set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
include(GoogleTest)

add_executable(test-${PROJECT_NAME}.exe testCounter.cpp testTimer.cpp testNode.cpp testProfiler.cpp test_report.cpp)
target_link_libraries(test-${PROJECT_NAME}.exe PUBLIC molpro::${PROJECT_NAME} gmock_main)
add_test(test-${PROJECT_NAME} test-${PROJECT_NAME}.exe)
gtest_discover_tests(test-${PROJECT_NAME}.exe)
