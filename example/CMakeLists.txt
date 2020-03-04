foreach (example profiler-example.cpp profiler-singleton-example.cpp profiler-fortran-example.F90)
    get_filename_component(name ${example} NAME_WE)
    add_executable(${name} ${example})
    target_link_libraries(${name} PUBLIC Profiler)
    if (MPI_CXX_FOUND)
        target_compile_definitions(${name} PRIVATE HAVE_MPI_H)
        add_test(NAME ${name} COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} ${MPIEXEC_MAX_NUMPROCS}
                ${MPIEXEC_PREFLAGS} ${CMAKE_CURRENT_BINARY_DIR}/${name} ${MPIEXEC_POSTFLAGS}
                WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    else()
        add_test(NAME ${name} COMMAND ${name} WORKING_DIRECTORY ${CMAKE_BINARY_DIR})
    endif ()
    set_tests_properties(${name} PROPERTIES TIMEOUT 60)
endforeach ()
