enable_testing()

add_executable(profiler-test main.cpp)
add_executable(profilerF-test mainF.F90)
target_link_libraries(profiler-test PUBLIC Profiler)
target_link_libraries(profilerF-test PUBLIC ProfilerF)

add_test(NAME profiler-test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
        ${MPIEXEC_PREFLAGS} profiler-test ${MPIEXEC_POSTFLAGS})
add_test(NAME profilerF-test COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
        ${MPIEXEC_PREFLAGS} profilerF-test ${MPIEXEC_POSTFLAGS})
