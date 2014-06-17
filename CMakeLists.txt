cmake_minimum_required(VERSION 2.8)

project(Profiler)
ENABLE_LANGUAGE(Fortran)

file (GLOB SRCS *.cpp *.h)
add_executable(Profiler.exe main.cpp Profiler.cpp Profiler.h ProfilerC.h)
add_executable(ProfilerF.exe mainF.F90 Profiler.cpp Profiler.h ProfilerC.h ProfilerF.F90)

find_package(Doxygen)
if(DOXYGEN_FOUND)
configure_file(${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in
${CMAKE_CURRENT_BINARY_DIR}/Doxyfile @ONLY)
add_custom_target(doc ALL
${DOXYGEN_EXECUTABLE} ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile
WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
COMMENT "Generating API documentation with Doxygen" VERBATIM
)
endif(DOXYGEN_FOUND)
