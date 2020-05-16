cmake_minimum_required(VERSION 3.14)
project(Profiler VERSION 1.0 LANGUAGES CXX)

########## Options ##########

if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    option(FORTRAN "Whether to build Profiler fortran binding" OFF)
    option(MPI "Whether to build Profiler::mpi" OFF)
    if (FORTRAN)
        enable_language(Fortran)
    endif ()
    if (MPI)
        message("Building parallel Profiler")
        find_package(MPI REQUIRED COMPONENTS MPICXX)
    endif ()
endif ()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
include(libconfig2)


########## Compiler Flags ##########

if (CMAKE_Fortran_COMPILER)
    message("Configuring Fortran support for ${CMAKE_CURRENT_SOURCE_DIR}")
    include(CheckFortranCompilerFlag)
    foreach (f "-fdefault-integer-8" "-i8")
        CHECK_Fortran_COMPILER_FLAG(${f} _fortran_flags)
        if (_fortran_flags)
            set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${f}")
        endif ()
        unset(_fortran_flags CACHE)
    endforeach ()
endif ()


########## Targets ##########

add_subdirectory(lib)
enable_testing()
add_subdirectory(example)

########## Documentation ##########

#if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
find_package(Doxygen)
if (DOXYGEN_FOUND)
    set(projectDocDir ${CMAKE_BINARY_DIR}/docs/${PROJECT_NAME})
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${projectDocDir}/Doxyfile @ONLY)
    add_custom_target(${PROJECT_NAME}-doc ALL
            DEPENDS ${projectDocDir}/html/index.html
            )
    file(GLOB headers README.md lib/*.h lib/serial/*.h lib/mpi/*.h lib/*.F90 ${CMAKE_CURRENT_BINARY_DIR}/lib/mpi/ProfilerMPI/Profiler.h)
    add_custom_command(OUTPUT ${projectDocDir}/html/index.html
            COMMAND ${DOXYGEN_EXECUTABLE} ${projectDocDir}/Doxyfile
            DEPENDS ${SRC_LIST} ${headers} ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt ${projectDocDir}/Doxyfile
            WORKING_DIRECTORY ${projectDocDir}
            COMMENT "Generating API documentation with Doxygen" VERBATIM
            )
endif (DOXYGEN_FOUND)
#endif ()
