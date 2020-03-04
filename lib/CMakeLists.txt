add_subdirectory(serial)
if(MPI)
    add_subdirectory(mpi)
endif()

set(PROFILER_SRCS
        Profiler.cpp
        ProfilerSingle.cpp
        )

if (CMAKE_Fortran_COMPILER_ID)
    list(APPEND PROFILER_SRCS ProfilerF.F90)
endif ()

set(PROFILER_PUBLIC_HEADERS
        ProfilerBackend.h
        ProfilerSingle.h
        )

add_library(ProfilerSerial STATIC ${PROFILER_SRCS})
set_target_properties(ProfilerSerial PROPERTIES
        PUBLIC_HEADER "${PROFILER_PUBLIC_HEADERS}"
        )
target_include_directories(ProfilerSerial PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/serial/ProfilerSerial>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        )
target_link_libraries(ProfilerSerial PUBLIC TempProfilerSerial)

if(MPI)
    add_library(ProfilerMPI STATIC ${PROFILER_SRCS})
    set_target_properties(ProfilerMPI PROPERTIES
            PUBLIC_HEADER "${PROFILER_PUBLIC_HEADERS}"
            )
    target_include_directories(ProfilerMPI PUBLIC
            $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
            $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mpi/ProfilerMPI>
            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
            )
    target_link_libraries(ProfilerMPI PUBLIC TempProfilerMPI)
endif()


add_library(Profiler::serial ALIAS ProfilerSerial)
add_library(Profiler::mpi ALIAS ProfilerMPI)
