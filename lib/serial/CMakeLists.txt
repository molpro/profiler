set(PROFILER_CLASS ProfilerSerial)
configure_file(../Profiler.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerSerial/Profiler.h)

set_property(TARGET ProfilerSerial APPEND PROPERTY
        PUBLIC_HEADER ${CMAKE_CURRENT_BINARY_DIR}/ProfilerSerial/Profiler.h
        )
target_include_directories(ProfilerSerial PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/ProfilerSerial>
        )
