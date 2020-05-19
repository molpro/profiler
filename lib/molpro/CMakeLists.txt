add_library(Profiler STATIC)
target_compile_features(Profiler PUBLIC cxx_std_11)

target_sources(Profiler PRIVATE ProfilerSingle.cpp ProfilerC.cpp)
if (CMAKE_Fortran_COMPILER)
    target_sources(Profiler PRIVATE ProfilerF.F90)
endif ()
set(pub_header ${CMAKE_CURRENT_SOURCE_DIR}/Profiler.h ${CMAKE_CURRENT_SOURCE_DIR}/ProfilerSingle.h
        ${CMAKE_CURRENT_SOURCE_DIR}/ProfilerC.h)
set_target_properties(Profiler PROPERTIES
        PUBLIC_HEADER "${pub_header}"
        )

add_subdirectory(Profiler)


add_library(Profiler::Profiler ALIAS Profiler)
target_include_directories(Profiler PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/../>
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/../>
        $<INSTALL_INTERFACE:include>
        )
install(TARGETS Profiler
        EXPORT ProfilerTargets
        PUBLIC_HEADER DESTINATION include/molpro
        PRIVATE_HEADER DESTINATION include/molpro/Profiler
        )
install(EXPORT ProfilerTargets
        NAMESPACE Profiler::
        DESTINATION lib/cmake
        FILE ProfilerConfig.cmake
        )
if (CMAKE_Fortran_COMPILER)
    install(FILES ${CMAKE_CURRENT_BINARY_DIR}/profilerf.mod DESTINATION include)
    file(CREATE_LINK ${CMAKE_CURRENT_BINARY_DIR}/profilerf.mod ${CMAKE_CURRENT_BINARY_DIR}/../profilerf.mod SYMBOLIC)
endif ()

