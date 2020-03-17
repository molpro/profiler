add_subdirectory(serial)

if (MPI_CXX_FOUND)
    message(STATUS "adding mpi subdirectory")
    add_subdirectory(mpi)
endif ()

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
        PUBLIC_HEADER "${PROFILER_PUBLIC_HEADERS};${CMAKE_CURRENT_BINARY_DIR}/serial/ProfilerSerial/Profiler.h"
        EXPORT_NAME serial
        )
target_include_directories(ProfilerSerial PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/serial/ProfilerSerial>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
        $<INSTALL_INTERFACE:include/ProfilerSerial>
        )
target_link_libraries(ProfilerSerial PUBLIC TempProfilerSerial)
add_library(Profiler::serial ALIAS ProfilerSerial)
install(TARGETS ProfilerSerial
        EXPORT ProfilerTargets
        PUBLIC_HEADER DESTINATION include/ProfilerSerial
        PRIVATE_HEADER DESTINATION include/ProfilerSerial
        )

if (TARGET TempProfilerMPI)
    add_library(ProfilerMPI STATIC ${PROFILER_SRCS})
    set_target_properties(ProfilerMPI PROPERTIES
            PUBLIC_HEADER "${PROFILER_PUBLIC_HEADERS};${CMAKE_CURRENT_BINARY_DIR}/mpi/ProfilerMPI/Profiler.h"
            EXPORT_NAME mpi
            )
    target_include_directories(ProfilerMPI PUBLIC
            $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
            $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mpi/ProfilerMPI>
            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<BUILD_INTERFACE:${CMAKE_Fortran_MODULE_DIRECTORY}>
            $<INSTALL_INTERFACE:include/ProfilerSerial>
            )
    target_link_libraries(ProfilerMPI PUBLIC TempProfilerMPI)
    add_library(Profiler::mpi ALIAS ProfilerMPI)
    install(TARGETS ProfilerMPI
            EXPORT ProfilerTargets
            PUBLIC_HEADER DESTINATION include/ProfilerMPI
            PRIVATE_HEADER DESTINATION include/ProfilerMPI
            )
endif ()

install(EXPORT ProfilerTargets
        NAMESPACE Profiler::
        DESTINATION lib/cmake/Profiler
        FILE ProfilerConfig.cmake
        )


