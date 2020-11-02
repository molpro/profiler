LibraryManager_Append(${PROJECT_NAME}
        SOURCES Timer.cpp Counter.cpp Profiler.cpp report.cpp
        PUBLIC_HEADER Timer.h Counter.h Profiler.h report.h Node.h WeakSingleton.h
        )
if (TARGET MPI::MPI_CXX)
    target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_CXX)
    target_compile_definitions(${PROJECT_NAME} PUBLIC MOLPRO_PROFILER_MPI)
endif ()
