if (CMAKE_Fortran_COMPILER_ID)
    set(PROFILER_FORTRAN_SRCS "ProfilerF.F90")
    set(PROFILER_FORTRAN 1)
endif ()
add_library(Profiler Profiler.cpp Profiler.h ${PROFILER_FORTRAN_SRCS})

if (MPI_CXX_FOUND)
    message(STATUS "Building with MPI")
    set(HAVE_MPI_H 1)
    target_link_libraries(Profiler PUBLIC MPI::MPI_CXX)
endif ()

if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "PGI")
    target_compile_definitions(Profile PRIVATE -DEIGEN_DONT_VECTORIZE)
endif ()


include(libconfig.cmake)
