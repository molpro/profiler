#add_configure_file(Profiler Profiler)

LibraryManager_Append(${PROJECT_NAME}
        SOURCES ProfilerSerial.cpp
        PUBLIC_HEADER ProfilerSerial.h
        )
#LibraryManager_AppendExternal(${PROJECT_NAME}
#        PUBLIC_HEADER ${CMAKE_CURRENT_BINARY_DIR}/Profiler-config.h
#        INCLUDE_DIR ${CMAKE_CURRENT_BINARY_DIR}/../../
#        )
target_include_directories(${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

if (TARGET MPI::MPI_CXX)
    if (Molpro_SOURCE_DIR)
        target_compile_definitions(${PROJECT_NAME}
                PRIVATE "MOLPRO_PROFILER_DEFAULT_COMMUNICATOR=MPI_Comm_f2c(PPIDD_Worker_comm())")
    else ()
        target_compile_definitions(${PROJECT_NAME} PRIVATE "MOLPRO_PROFILER_DEFAULT_COMMUNICATOR=MPI_COMM_WORLD")
    endif ()
    if (TARGET ppidd::ppidd)
        target_link_libraries(${PROJECT_NAME} PRIVATE ppidd::ppidd)
    endif ()
    if (TARGET molpro::memory)
        target_link_libraries(${PROJECT_NAME} PRIVATE molpro::memory)
    endif ()

    LibraryManager_Append(${PROJECT_NAME}
            SOURCES ProfilerMPI.cpp
            PUBLIC_HEADER ProfilerMPI.h
            )
    target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_CXX)
    target_compile_definitions(${PROJECT_NAME} PUBLIC MOLPRO_PROFILER_MPI)
endif ()


