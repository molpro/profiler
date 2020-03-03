if (CMAKE_Fortran_COMPILER_ID)
    set(PROFILER_FORTRAN_SRCS "ProfilerF.F90")
    set(PROFILER_FORTRAN 1)
endif ()
#add_library(Profiler  ProfilerMPI.cpp ProfilerMPI.h ProfilerSingle.h
#                     ProfilerSingle.cpp Profiler.cpp Profiler.h.in ${PROFILER_FORTRAN_SRCS} ProfilerBackend.h)

add_library(Profiler STATIC Profiler.cpp ProfilerSerial.cpp ProfilerSingle.cpp ${PROFILER_FORTRAN_SRCS})
set_target_properties(Profiler PROPERTIES
        PUBLIC_HEADER ${CMAKE_CURRENT_BINARY_DIR}/Profiler.h
        PUBLIC_HEADER ProfilerSerial.h
        PUBLIC_HEADER ProfilerSingle.h
        )

set(PROFILER_CLASS ProfilerSerial)

if (Molpro_SOURCE_DIR)
    target_compile_definitions(Profiler PRIVATE MOLPRO)
    target_link_libraries(Profiler PRIVATE ppidd::ppidd memory::memory)
endif ()

if (MPI_CXX_FOUND)
    message(STATUS "Building with MPI")
    set(HAVE_MPI_H 1)
    set(PROFILER_CLASS ProfilerMPI)
    target_compile_definitions(Profiler PUBLIC HAVE_MPI_H)
    target_compile_definitions(Profiler PUBLIC PROFILER_MPI)
    target_sources(Profiler PRIVATE ProfilerMPI.cpp)
    set_target_properties(Profiler PROPERTIES PRIVATE_HEADER ProfilerMPI.h)
    target_link_libraries(Profiler PUBLIC MPI::MPI_CXX)
endif ()

configure_file(Profiler.h.in ${CMAKE_CURRENT_BINARY_DIR}/Profiler.h)

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "PGI")
    target_compile_definitions(Profiler PRIVATE -DEIGEN_DONT_VECTORIZE)
endif ()

if (Molpro_SOURCE_DIR)
    get_dependency(ppidd)
    get_dependency(memory)
    configure_library(${PROJECT_NAME} ppidd memory)
else ()
    configure_library(${PROJECT_NAME})
endif ()
