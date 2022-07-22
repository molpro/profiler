cmake_minimum_required(VERSION 3.14)
project(Profiler CXX)

include(FetchContent)
FetchContent_Declare(
        LibraryManager
        GIT_REPOSITORY https://gitlab.com/molpro/librarymanager.git
        GIT_TAG fef21c35ac00abea4d810ee8c8dbc34dcf8fa2e8
)
FetchContent_Declare(
        dependency_manager
        GIT_REPOSITORY https://gitlab.com/dependencymanager/dependency-manager.git
        GIT_TAG 0.1.5
)
FetchContent_MakeAvailable(LibraryManager)

LibraryManager_Project(FORTRAN_OPTION MPI_OPTION)

if ("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
    set(CMAKE_CXX_STANDARD 11)
endif ()

if (MPI)
    message("Building parallel Profiler")
    find_package(MPI COMPONENTS CXX)
endif ()

add_subdirectory(src)
if ("${CMAKE_SOURCE_DIR}" STREQUAL "${CMAKE_CURRENT_SOURCE_DIR}")
    enable_testing()
    add_subdirectory(test)
    add_subdirectory(example)
    add_subdirectory(benchmark)
endif ()

FetchContent_MakeAvailable(dependency_manager)
add_subdirectory(doc)
