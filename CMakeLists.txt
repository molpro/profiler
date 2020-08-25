cmake_minimum_required(VERSION 3.14)
project(Profiler VERSION 1.0 LANGUAGES CXX)

include(FetchContent)
FetchContent_Declare(
        LibraryManager
        GIT_REPOSITORY https://gitlab+deploy-token-178345:RQZa2rmdwnV7Hdwg8XEq@gitlab.com/molpro/LibraryManager.git
        GIT_TAG master
)
FetchContent_Declare(
        dependency_manager
        GIT_REPOSITORY https://gitlab.com/dependencymanager/dependency-manager.git
        GIT_TAG master
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
    add_subdirectory(example)
    add_subdirectory(benchmark)
endif ()

########## Documentation ##########

FetchContent_MakeAvailable(dependency_manager)

include(DependencyManagerDocs)
DependencyManagerDocs_Add(Profiler
        FILES README.md
        DOXYFILE Doxyfile
        DOC_URL https://molpro.gitlab.io/profiler/${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR})
