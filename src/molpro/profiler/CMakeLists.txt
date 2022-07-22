LibraryManager_Append(${PROJECT_NAME}
        SOURCES Profiler.cpp Timer.cpp Counter.cpp report.cpp dotgraph.cpp mpi.h
        PUBLIC_HEADER Profiler.h Timer.h Counter.h report.h Node.h WeakSingleton.h SortBy.h dotgraph.h
        )
if (TARGET MPI::MPI_CXX)
    target_link_libraries(${PROJECT_NAME} PUBLIC MPI::MPI_CXX)
    target_compile_definitions(${PROJECT_NAME} PUBLIC MOLPRO_PROFILER_MPI)
endif ()
