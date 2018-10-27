#add_definitions(-DMAIN)
add_library(Profiler Profiler.cpp Profiler.h ProfilerF.F90)
target_include_directories(Profiler INTERFACE ${CMAKE_CURRENT_SOURCE_DIR})
target_include_directories(Profiler INTERFACE ${CMAKE_CURRENT_BINARY_DIR})

if (MPI_CXX_FOUND AND MPI_FORTRAN_FOUND)
    target_include_directories(Profiler PUBLIC ${MPI_CXX_INCLUDE_PATH})
    target_link_libraries(Profiler PUBLIC MPI::MPI_CXX)
    target_link_libraries(Profiler PUBLIC MPI::MPI_Fortran)
endif ()

