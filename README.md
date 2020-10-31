Welcome to Profiler!
====================

# Overview

Framework for timing code sections in serial and parallel implemented in C++ with C and Fortran wrappers.
The profiler works by constructing a call tree and accumulating call count, operation count, and timing
duration in each node. This profile tree can be analysed and printed.

**NOTE** we are transitioning to a new structure, and the interface is about to change.

# Features
  * Constructs profiler call tree and accumulates statistics for each node
  * Multiple call trees can be processed, allowing simple use in parallel applications
  * management of global Profiler objects using a singleton class
  * includes Fotran and C bindings
  * easily integration into the build system with CMake
  
# CMake options
  * MPI
    - TRY: build mpi library if MPI is available
    - ON: build mpi, raising an error if MPI is not available
    - OFF: no mpi
  * Fortran
    - ON/OFF: whether to build Fortran90 bindings
    
# Using Profiler in your project
Name of CMake library target is **molpro::Profiler**

Profiler is intended to be used by adding it to your CMake build with FetchContent,
and linking it with target_link_libraries().

CMake options do not need to be set and the correct targets will be built
based on whether MPI package was already found and 
whether Fortran compilers are available.

If Profiler is built with MPI, than default Profiler class can still be made serial
by unsetting compiler definition *PROFILER_MPI*.

# List of Contributors

Prof. Peter J. Knowles

Marat Sibaev

Iakov Polyak

# License

MIT License
Copyright (c) 2020 Peter James Knowles
