target_sources(Profiler PRIVATE ProfilerSerial.cpp)
add_configure_file(Profiler Profiler)
get_target_property(priv_head Profiler PRIVATE_HEADER)
if(NOT priv_head)
    set(priv_head "")
endif()
list(APPEND priv_head ${CMAKE_CURRENT_SOURCE_DIR}/ProfilerSerial.h ${CMAKE_CURRENT_BINARY_DIR}/Profiler-config.h)
set_target_properties(Profiler PROPERTIES
        PRIVATE_HEADER "${priv_head}"
        )

if (TARGET MPI::MPI_CXX)
    set(PROFILER_DEFAULT_COMMUNICATOR MPI_COMM_WORLD)
    if (Molpro_SOURCE_DIR)
        target_compile_definitions(ProfilerMPI PRIVATE MOLPRO)
        target_link_libraries(ProfilerMPI PRIVATE ppidd::ppidd memory::memory)
        set(PROFILER_DEFAULT_COMMUNICATOR "MPI_Comm_f2c(PPIDD_Worker_comm())")
        set(EXTRA_MPI_INCLUDE "#include <ppidd.h>")
    endif ()
    configure_file(ProfilerMPIConfig.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h)

    target_link_libraries(Profiler PUBLIC MPI::MPI_CXX)
    target_sources(Profiler PRIVATE ProfilerMPI.cpp)
    get_target_property(priv_head Profiler PRIVATE_HEADER)
    if(NOT priv_head)
        set(priv_head "")
    endif()
    list(APPEND priv_head ${CMAKE_CURRENT_SOURCE_DIR}/ProfilerMPI.h ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h)
    set_target_properties(Profiler PROPERTIES
            PRIVATE_HEADER "${priv_head}"
            )
    target_compile_definitions(Profiler PUBLIC PROFILER_MPI)
endif ()
