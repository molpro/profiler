FetchContent_MakeAvailable(googletest)
include(GoogleTest)

add_executable(test-${PROJECT_NAME}.exe testCounter.cpp testTimer.cpp testNode.cpp)
target_link_libraries(test-${PROJECT_NAME}.exe PUBLIC molpro::${PROJECT_NAME} gmock_main)
add_test(test-${PROJECT_NAME} test-${PROJECT_NAME}.exe)
gtest_discover_tests(test-${PROJECT_NAME}.exe)
