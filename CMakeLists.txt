project(Profiler Fortran CXX)
cmake_minimum_required(VERSION 3.1)
set (CMAKE_CXX_STANDARD 11)

add_definitions(-DMAIN)
add_executable(Profiler.exe main.cpp Profiler.cpp Profiler.h)
add_executable(ProfilerF.exe ProfilerF.F90 Profiler.cpp Profiler.h)
#set_property(TARGET ProfilerF.exe PROPERTY LINKER_LANGUAGE Fortran) 
#target_link_libraries(ProfilerF.exe -lstdc++)

# get rid of the next lines if you want serial
find_package(MPI REQUIRED)
include_directories(${MPI_CXX_INCLUDE_PATH})
add_definitions(-DMPI2)
target_link_libraries(Profiler.exe ${MPI_CXX_LIBRARIES})
target_link_libraries(ProfilerF.exe ${MPI_CXX_LIBRARIES})

find_package(Doxygen)
if(DOXYGEN_FOUND)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
add_custom_target(${PROJECT_NAME}-doc ALL
    DEPENDS  ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
    )
file(GLOB headers *.h *.fh)
add_custom_command(OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/html/index.html
COMMAND ${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
DEPENDS ${SRC_LIST} ${headers} ${CMAKE_CURRENT_SOURCE_DIR}/CMakeLists.txt ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
COMMENT "Generating API documentation with Doxygen" VERBATIM
)
endif(DOXYGEN_FOUND)
