add_library(TempProfilerSerial STATIC ProfilerSerial.cpp)
set_target_properties(TempProfilerSerial PROPERTIES PUBLIC_HEADER "ProfilerSerial.h")

set(PROFILER_CLASS ProfilerSerial)
configure_file(../Profiler.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerSerial/Profiler.h)
target_include_directories(TempProfilerSerial PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:include/ProfilerSerial>
        )
add_configure_file(TempProfilerSerial Profiler)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "PGI")
    target_compile_definitions(Profiler PRIVATE -DEIGEN_DONT_VECTORIZE)
endif ()

install(TARGETS TempProfilerSerial
        EXPORT ProfilerTargets
        PUBLIC_HEADER DESTINATION include/ProfilerSerial
        PRIVATE_HEADER DESTINATION include/ProfilerSerial
        )

