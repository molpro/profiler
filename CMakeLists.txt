project(Profiler Fortran CXX)
cmake_minimum_required(VERSION 3.1)
set(CMAKE_CXX_STANDARD 11)

########## Compiler Flags ##########

#FIXME: need a better procedure
# get rid of the next lines if you want serial
if (${CMAKE_CURRENT_SOURCE_DIR} STREQUAL ${CMAKE_SOURCE_DIR})
    find_package(MPI REQUIRED)
    if (MPI_CXX_FOUND AND MPI_FORTRAN_FOUND)
        add_definitions(-DHAVE_MPI_H)
        message(STATUS "MPI C++ and Fortran libraries found")
    endif ()
endif ()


########## Targets ##########

add_subdirectory(lib)
add_subdirectory(test)

########## Documentation ##########

find_package(Doxygen)
if (DOXYGEN_FOUND)
    configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
    add_custom_target(${PROJECT_NAME}-doc ALL
            DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
            )
    file(GLOB headers *.h *.fh)
    add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
            COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
            DEPENDS ${SRC_LIST} ${headers} ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
            WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
            COMMENT "Generating API documentation with Doxygen" VERBATIM
            )
endif (DOXYGEN_FOUND)
