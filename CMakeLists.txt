project(Profiler CXX)
cmake_minimum_required(VERSION 3.1)
set(CMAKE_CXX_STANDARD 11)
if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    option(PROFILER_FORTRAN "Whether to build symmetry_matrix fortran binding" ON)
endif ()
if (PROFILER_FORTRAN)
    enable_language(Fortran)
endif ()

########## Compiler Flags ##########

# get rid of the next lines if you want serial
if (${CMAKE_CURRENT_SOURCE_DIR} STREQUAL ${CMAKE_SOURCE_DIR})
    find_package(MPI REQUIRED)
endif ()
if (MPI_CXX_FOUND)
    add_definitions(-DHAVE_MPI_H)
    message(STATUS "Building with MPI")
endif ()

include(CheckCXXCompilerFlag)
CHECK_CXX_COMPILER_FLAG("-Wall" _Wall)
if (_Wall)
    set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")
endif ()
if ("${CMAKE_CXX_COMPILER_ID}" STREQUAL "PGI")
    add_definitions(-DEIGEN_DONT_VECTORIZE)
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
    CHECK_Fortran_COMPILER_FLAG("-Wall" _Wallf)
    if (_Wallf)
        set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wall")
    endif ()
endif ()


########## Targets ##########

add_subdirectory(lib)
add_subdirectory(example)

########## Documentation ##########

find_package(Doxygen)
if (DOXYGEN_FOUND)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target(${PROJECT_NAME}-doc ALL
            DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
            )
    file(GLOB headers lib/*.h lib/*.F90)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
            COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
            DEPENDS ${SRC_LIST} ${headers} ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Generating API documentation with Doxygen" VERBATIM
            )
endif (DOXYGEN_FOUND)
