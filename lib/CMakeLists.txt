if (CMAKE_Fortran_COMPILER_ID)
    set(PROFILER_FORTRAN_SRCS "ProfilerF.F90")
endif ()
add_library(Profiler Profiler.cpp Profiler.h ${PROFILER_FORTRAN_SRCS})
target_include_directories(Profiler INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(Profiler INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

if (MPI_CXX_FOUND)
    target_include_directories(Profiler PUBLIC ${MPI_CXX_INCLUDE_PATH})
    target_link_libraries(Profiler PUBLIC MPI::MPI_CXX)
endif ()

