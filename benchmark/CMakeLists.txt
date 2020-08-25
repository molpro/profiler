add_executable(profiler-benchmark-cpp benchmark-cpp.cpp)
target_link_libraries(profiler-benchmark-cpp molpro::Profiler)
target_compile_features(profiler-benchmark-cpp PUBLIC cxx_std_14)

add_executable(profiler-benchmark-f benchmark-f.F90)
target_link_libraries(profiler-benchmark-f molpro::Profiler)
