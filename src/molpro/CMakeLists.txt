LibraryManager_Append(${PROJECT_NAME}
        SOURCES ProfilerSingle.cpp ProfilerC.cpp
        PUBLIC_HEADER Profiler.h ProfilerSingle.h ProfilerC.h
        )
target_compile_features(${PROJECT_NAME} PUBLIC cxx_std_14)
set_target_properties(${PROJECT_NAME} PROPERTIES CXX_EXTENSIONS OFF)

add_subdirectory(${PROJECT_NAME})

if (FORTRAN)
    LibraryManager_Append(${PROJECT_NAME} SOURCES ProfilerF.F90)
endif ()

