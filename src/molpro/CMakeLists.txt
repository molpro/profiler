LibraryManager_Append(${PROJECT_NAME}
        SOURCES  ProfilerC.cpp
        PUBLIC_HEADER Profiler.h ProfilerC.h
        )
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_11)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_EXTENSIONS OFF)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_STANDARD_REQUIRED ON)

add_subdirectory(profiler)

if (FORTRAN)
    LibraryManager_Append(${PROJECT_NAME} SOURCES ProfilerF.F90)
endif ()

if (TARGET ppidd::ppidd)
    message(STATUS "molpro::Profiler with PPIDD")
    target_link_libraries(${PROJECT_NAME} PUBLIC ppidd::ppidd)
    target_compile_definitions(${PROJECT_NAME} PRIVATE HAVE_PPIDD_H)
endif ()
