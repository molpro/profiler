LibraryManager_Append(${PROJECT_NAME}
        SOURCES ProfilerSerial.cpp
        PUBLIC_HEADER ProfilerSerial.h
        )
target_include_directories(${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

if (TARGET MPI::MPI_CXX)
    if (TARGET ppidd::ppidd)
        target_link_libraries(${PROJECT_NAME} PRIVATE ppidd::ppidd)
    endif ()
    if (TARGET molpro::memory)
        target_link_libraries(${PROJECT_NAME} PRIVATE molpro::memory)
        target_compile_definitions(${PROJECT_NAME} PRIVATE "PROFILER_MEMORY")
    endif ()

    LibraryManager_Append(${PROJECT_NAME}
            SOURCES ProfilerMPI.cpp
            PUBLIC_HEADER ProfilerMPI.h
            )
    target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_CXX)
    target_compile_definitions(${PROJECT_NAME} PUBLIC MOLPRO_PROFILER_MPI)
endif ()

add_subdirectory(Tree)

