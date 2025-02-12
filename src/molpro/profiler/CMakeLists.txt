LibraryManager_Append(${PROJECT_NAME}
        SOURCES Profiler.cpp Timer.cpp Counter.cpp report.cpp dotgraph.cpp
        PUBLIC_HEADER Profiler.h Timer.h Counter.h report.h Node.h WeakSingleton.h SortBy.h dotgraph.h
        )
if (TARGET MPI::MPI_CXX)
#    target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_CXX)
target_compile_definitions(${PROJECT_NAME} PUBLIC HAVE_MPI_H)
    target_compile_definitions(${PROJECT_NAME} PUBLIC MOLPRO_PROFILER_MPI)
endif ()

DependencyManager_Populate(utilities)
target_link_libraries(${PROJECT_NAME} PUBLIC molpro::utilities)