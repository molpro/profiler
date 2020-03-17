add_library(ProfilerSerial STATIC)
target_sources(ProfilerSerial
        PRIVATE ProfilerSerial.cpp Profiler.cpp ProfilerSingle.cpp
        )
set_target_properties(ProfilerSerial PROPERTIES
        PUBLIC_HEADER "ProfilerSerial.h;ProfilerBackend.h;ProfilerSingle.h"
        EXPORT_NAME serial
        )
if (CMAKE_Fortran_COMPILER_ID)
    target_sources(ProfilerSerial PRIVATE ProfilerF.F90)
endif ()
add_configure_file(ProfilerSerial Profiler)
target_include_directories(ProfilerSerial PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
        $<INSTALL_INTERFACE:include/ProfilerSerial>
        )

add_subdirectory(serial)

add_library(Profiler::serial ALIAS ProfilerSerial)
install(TARGETS ProfilerSerial
        EXPORT ProfilerTargets
        PUBLIC_HEADER DESTINATION include/ProfilerSerial
        PRIVATE_HEADER DESTINATION include/ProfilerSerial
        )

# ProfilerMPI is a duplicate of ProfilerSerial with a different Profiler.h
if (TARGET MPI::MPI_CXX)
    add_library(ProfilerMPI STATIC "")
    get_target_property(srcs ProfilerSerial SOURCES)
    get_target_property(pub_head ProfilerSerial PUBLIC_HEADER)
    list(FILTER pub_head EXCLUDE REGEX "Profiler.h$")
    target_sources(ProfilerMPI
            PRIVATE ${srcs}
            )
    set_target_properties(ProfilerMPI PROPERTIES
            PUBLIC_HEADER "${pub_head}"
            EXPORT_NAME mpi
            )
    target_include_directories(ProfilerMPI PUBLIC
            $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>
            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
            $<INSTALL_INTERFACE:include/ProfilerMPI>
            )
    target_compile_definitions(ProfilerMPI PUBLIC PROFILER_MPI)
    target_link_libraries(ProfilerMPI PUBLIC MPI::MPI_CXX)
    add_subdirectory(mpi)
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


