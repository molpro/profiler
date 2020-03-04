if (NOT MPI_CXX_FOUND)
    message(FATAL_ERROR "Instructed to build with MPI, but MPI C++ compiler was not found")
endif ()

message(STATUS "Building with MPI")

add_library(TempProfilerMPI STATIC ProfilerMPI.cpp)
set_target_properties(TempProfilerMPI PROPERTIES
        PRIVATE_HEADER "ProfilerMPI.h;ProfilerMPIConfig.h."
        )
target_include_directories(TempProfilerMPI PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        )

target_link_libraries(TempProfilerMPI PUBLIC TempProfilerSerial MPI::MPI_CXX)
target_compile_definitions(TempProfilerMPI PRIVATE PROFILER_MPI)

set(PROFILER_CLASS ProfilerMPI)
configure_file(../Profiler.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPI/Profiler.h)

set(PROFILER_DEFAULT_COMMUNICATOR MPI_COMM_WORLD)
if (Molpro_SOURCE_DIR)
    target_compile_definitions(TempProfilerMPI PRIVATE MOLPRO)
    target_link_libraries(TempProfilerMPI PRIVATE ppidd::ppidd memory::memory)
    set(PROFILER_DEFAULT_COMMUNICATOR "MPI_Comm_f2c(PPIDD_Worker_comm())")
    set(EXTRA_MPI_INCLUDE "#include <ppidd.h>")
endif ()

configure_file(ProfilerMPIConfig.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h)
