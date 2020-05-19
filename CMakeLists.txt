cmake_minimum_required(VERSION 3.14)
project(Profiler VERSION 1.0 LANGUAGES CXX)

########## Options ##########

if (CMAKE_CURRENT_SOURCE_DIR STREQUAL CMAKE_SOURCE_DIR)
    option(FORTRAN "Whether to build Profiler fortran binding" OFF)
    option(MPI "Whether to build Profiler::mpi" OFF)
    if (FORTRAN)
        enable_language(Fortran)
    endif ()
    if (MPI)
        message("Building parallel Profiler")
        find_package(MPI REQUIRED COMPONENTS MPICXX)
    endif ()
endif ()

list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")
include(libconfig2)


########## Compiler Flags ##########

if (CMAKE_Fortran_COMPILER)
    message("Configuring Fortran support for ${CMAKE_CURRENT_SOURCE_DIR}")
    include(CheckFortranCompilerFlag)
    foreach (f "-fdefault-integer-8" "-i8")
        CHECK_Fortran_COMPILER_FLAG(${f} _fortran_flags)
        if (_fortran_flags)
            set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${f}")
        endif ()
        unset(_fortran_flags CACHE)
    endforeach ()
endif ()


########## Targets ##########

add_subdirectory(lib)
enable_testing()
add_subdirectory(example)

########## Documentation ##########

include(FetchContent)
FetchContent_Declare(
        molpro_cmake_modules
        GIT_REPOSITORY git@gitlab.com:Knowles-group/molpro-cmake-modules.git
        GIT_TAG 89c978c5fbcbf489cee73c52ed09c4c8c31955d5
)
FetchContent_MakeAvailable(molpro_cmake_modules)

include(MolproDocs)
MolproDocs_Add(Profiler
        FILES README.md
        DOXYFILE Doxyfile
        DOC_URL https://mxs301.gitlab.io/profiler/update_doxygen)
