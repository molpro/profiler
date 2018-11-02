enable_testing()

add_executable(profiler-example profiler-example.cpp)
add_executable(profiler-fortran-example profiler-fortran-example.F90)
target_link_libraries(profiler-example PUBLIC Profiler)
target_link_libraries(profiler-fortran-example PUBLIC Profiler)

add_test(NAME profiler-example COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
        ${MPIEXEC_PREFLAGS} profiler-example ${MPIEXEC_POSTFLAGS})
add_test(NAME profiler-fortran-example COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
        ${MPIEXEC_PREFLAGS} profiler-fortran-example ${MPIEXEC_POSTFLAGS})
set_tests_properties(profiler-example PROPERTIES TIMEOUT 60)
set_tests_properties(profiler-fortran-example PROPERTIES TIMEOUT 60)
