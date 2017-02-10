project(Profiler)
cmake_minimum_required(VERSION 3.1)

ENABLE_LANGUAGE(Fortran)

find_package(MPI REQUIRED)
include_directories(${MPI_CXX_INCLUDE_PATH})

add_definitions(-DMAIN)
add_definitions(-DMPI2) # get rid of this if you want serial
add_executable(Profiler.exe main.cpp Profiler.cpp Profiler.h)
add_executable(ProfilerF.exe Profiler.cpp Profiler.h ProfilerF.F90)
target_link_libraries(Profiler.exe ${MPI_CXX_LIBRARIES})
target_link_libraries(ProfilerF.exe ${MPI_CXX_LIBRARIES})

find_package(Doxygen)
if(DOXYGEN_FOUND)
  SET( doxyfile_in          ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in     )
  SET( doxyfile             ${PROJECT_BINARY_DIR}/Doxyfile              )
  SET( doxy_html_index_file ${CMAKE_CURRENT_BINARY_DIR}/html/index.html )
  SET( doxy_output_root     ${CMAKE_CURRENT_BINARY_DIR}                 ) # Pasted into Doxyfile.in
  SET( doxy_input           ${PROJECT_SOURCE_DIR}                   ) # Pasted into Doxyfile.in
 
  CONFIGURE_FILE( ${doxyfile_in} ${doxyfile} @ONLY )
 
  ADD_CUSTOM_COMMAND( OUTPUT ${doxy_html_index_file}
                      COMMAND ${DOXYGEN_EXECUTABLE} ${doxyfile}
                      MAIN_DEPENDENCY ${doxyfile} ${doxyfile_in}
                      COMMENT "Generating HTML documentation")
 
  ADD_CUSTOM_TARGET( doc ALL DEPENDS ${doxy_html_index_file} )
 
  INSTALL( DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/html DESTINATION share/doc )
endif(DOXYGEN_FOUND)
