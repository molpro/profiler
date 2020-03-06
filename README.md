Profiler Documentation                         {#mainpage}
======================

Framework for timing code sections in serial and parallel using Fortran90, C or C++.

### Features
  * serial and parallel versions with consistent interface via Profiler.h
  * management of global Profiler objects using a singleton class
  * includes Fotran and C bindings
  
### CMake options
  * MPI
    - TRY: build mpi library if MPI is available
    - ON: build mpi, raising an error if MPI is not available
    - OFF: no mpi
  * Fortran
    - ON/OFF: whether to build Fortran90 bindings
    
### Using Profiler in your project
Profiler is intended to be used by adding it to your CMake build with FetchContent,
and linking it with target_link_libraries().

There are two targets,
  * Profiler::serial
    - serial library, where Profiler in Profiler.h is an alias to ProfilerSerial
  * Profiler::mpi
    - parallel library, where Profiler in Profiler.h is an alias to ProfilerMPI
    - ProfilerSerial class is still accessible through ProfilerSerial.h

CMake options do not need to be set and the correct targets will be built
based on whether MPI package was already found and 
whether Fortran compilers are available.

### Author
Peter Knowles

### Contributors
Marat Sibaev