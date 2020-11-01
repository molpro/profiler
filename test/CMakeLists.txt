FetchContent_MakeAvailable(googletest)
include(GoogleTest)

add_executable(test-${PROJECT_NAME}.exe testCounter.cpp)
target_link_libraries(test-${PROJECT_NAME}.exe PUBLIC molpro::${PROJECT_NAME} gmock_main)
gtest_discover_tests(test-${PROJECT_NAME}.exe)
