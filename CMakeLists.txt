cmake_minimum_required(VERSION 3.14)
project(Profiler LANGUAGES CXX)

include(FetchContent)
FetchContent_Declare(
        LibraryManager
        GIT_REPOSITORY https://gitlab.com/molpro/librarymanager.git
        GIT_TAG 0.6.9
)
FetchContent_Declare(
        dependency_manager
        GIT_REPOSITORY https://gitlab.com/dependencymanager/dependency-manager.git
        GIT_TAG 0.1.5
)
set(INSTALL_GTEST OFF CACHE BOOL "" FORCE)
FetchContent_Declare(
        googletest
        GIT_REPOSITORY https://github.com/google/googletest.git
        GIT_TAG "release-1.10.0"
)
FetchContent_MakeAvailable(LibraryManager)

LibraryManager_Project(FORTRAN_OPTION MPI_OPTION)

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
