cmake_minimum_required(VERSION 3.14)
project(profiler CXX)

include(FetchContent)
FetchContent_Declare(
        LibraryManager
        GIT_REPOSITORY https://github.com/molpro/library-manager.git
        GIT_TAG 0.13.0
)
FetchContent_Declare(
        dependency_manager
        GIT_REPOSITORY https://github.com/molpro/dependency-manager.git
        GIT_TAG 0.2.0
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
