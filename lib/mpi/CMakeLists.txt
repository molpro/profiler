set(PROFILER_CLASS ProfilerMPI)
configure_file(../Profiler.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPI/Profiler.h)

set(PROFILER_DEFAULT_COMMUNICATOR MPI_COMM_WORLD)
if (Molpro_SOURCE_DIR)
    target_compile_definitions(ProfilerMPI PRIVATE MOLPRO)
    target_link_libraries(ProfilerMPI PRIVATE ppidd::ppidd memory::memory)
    set(PROFILER_DEFAULT_COMMUNICATOR "MPI_Comm_f2c(PPIDD_Worker_comm())")
    set(EXTRA_MPI_INCLUDE "#include <ppidd.h>")
endif ()
configure_file(ProfilerMPIConfig.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h)

target_sources(ProfilerMPI PRIVATE ProfilerMPI.cpp)
set_property(TARGET ProfilerMPI APPEND PROPERTY
        PUBLIC_HEADER "${CMAKE_CURRENT_SOURCE_DIR}/ProfilerMPI.h;${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPI/Profiler.h;${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h"
        )
target_include_directories(ProfilerMPI PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPI>
        )
