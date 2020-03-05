cmake_minimum_required(VERSION 3.11)
project(Profiler VERSION 1.0 LANGUAGES CXX)
set(CMAKE_CXX_STANDARD 11)

########## Options ##########

if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    option(FORTRAN "Whether to build Profiler fortran binding" ON)
    set(MPI "TRY" CACHE STRING "Whether to build with MPI support: TRY - use mpi if found, ON - mpi required, OFF - no mpi.")
endif ()

if (FORTRAN)
    enable_language(Fortran)
endif ()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
include(libconfig2)


########## Compiler Flags ##########

string(TOUPPER ${MPI} MPI_UPPER)
if (MPI_UPPER STREQUAL "TRY")
    find_package(MPI COMPONENTS CXX)
    message(WARNING "Instructed to try and build with MPI, but MPI C++ compiler was not found")
elseif (MPI_UPPER STREQUAL "ON")
    find_package(MPI REQUIRED COMPONENTS CXX)
endif ()

if (CMAKE_Fortran_COMPILER_ID)
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

if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    find_package(Doxygen)
    if (DOXYGEN_FOUND)
        configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
        add_custom_target(${PROJECT_NAME}-doc ALL
                DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
                )
        file(GLOB headers README.md lib/*.h lib/serial/*.h lib/mpi/*.h lib/*.F90)
        add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
                COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
                DEPENDS ${SRC_LIST} ${headers} ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
                WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                COMMENT "Generating API documentation with Doxygen" VERBATIM
                )
    endif (DOXYGEN_FOUND)
endif ()
