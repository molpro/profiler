add_configure_file(Profiler Profiler)

LibraryManager_Append(${PROJECT_NAME}
        SOURCES ProfilerSerial.cpp
        PUBLIC_HEADER ProfilerSerial.h ${CMAKE_CURRENT_BINARY_DIR}/Profiler-config.h
        )

if(TARGET MPI::MPI_CXX)
    set(MOLPRO_PROFILER_DEFAULT_COMMUNICATOR MPI_COMM_WORLD)
    if (Molpro_SOURCE_DIR)
        target_compile_definitions(${PROJECT_NAME} PRIVATE MOLPRO)
        target_link_libraries(${PROJECT_NAME} PRIVATE ppidd::ppidd molpro::memory)
        set(MOLPRO_PROFILER_DEFAULT_COMMUNICATOR "MPI_Comm_f2c(PPIDD_Worker_comm())")
        set(EXTRA_MPI_INCLUDE "#include <ppidd.h>")
    endif ()
    configure_file(ProfilerMPIConfig.h.in ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h)

    LibraryManager_Append(${PROJECT_NAME}
            SOURCES ProfilerMPI.cpp
            PUBLIC_HEADER ProfilerMPI.h ${CMAKE_CURRENT_BINARY_DIR}/ProfilerMPIConfig.h
            )
    target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_CXX)
    target_compile_definitions(${PROJECT_NAME} PUBLIC MOLPRO_PROFILER_MPI)
endif()


