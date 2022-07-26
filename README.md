Profiler
========

Full documentation can be found here 
[https://molpro.github.io/profiler](https://molpro.github.io/profiler).

# Overview

Framework for timing sections of code in serial and parallel, implemented in C++ with C and Fortran wrappers.

**Disclaimer**, this is an instrumental profiler and timing statements have to be added to the source code.
This can degrade overall performance and distort real profile if attempting to time tight loops.

The profiler works by constructing a call tree and accumulating call count, operation count, and timing
duration in each node. Optionally, but typically with significantly greater overhead, CPU time can be recorded also.
The maximum depth of profiler tree can also be limited, resulting in any calls to start() that would grow the tree beyond
the limit to do nothing. This allows Profiler to be used in production code without degrading performance.
The profile tree can be analysed and printed.

In the following simple example, calling *start()* on profiler moves down the call tree and starts timing and calling
*stop()* moves up to the parent and stops timing. 
The same effect can be achieved on a scope level by calling *push()* which returns a proxy that starts profiling
on construction and stops on destruction.
```cpp
#include <molpro/Profiler.h>

void read_input(Profiler&);
void restore_backup(Profiler&);
void operation1();
void operation2();

int main(){
    auto prof = molpro::Profiler{"main()"};
    {
        auto p = prof.push("initialise()");
        // ....
        read_input(prof);
        restore_backup(prof);
    }
    prof.start("perform_calculation()");
    // ....
    for (size_t i = 0; i < 2; ++i){
        {
            auto p = prof.push("operation1()");
            operation1();
        }
        {
            auto p = prof.push("operation2()");
            operation2();
        }
    }
    prof.stop();
    std::cout << prof << std::endl;
    // Or directly call report()
    report(prof, std::cout);
    return 0;
}
```

Here is a visual representation of the profiler call tree during execution.

<img src="doc/profiler.gif" width="800" />

At the end of calculation the profiler can be analysed and printed out.
By default, the call tree is sorted by wall time at each level and cumulative times are used.
```
Profiler "main()" (cumulative)
All                   : calls=1, wall=18
.perform_calculation(): calls=1, wall=10
..operation2()        : calls=2, wall=4
..operation1()        : calls=2, wall=4
.initialise()         : calls=1, wall=6
..restore_backup()    : calls=1, wall=2
..read_input()        : calls=1, wall=2
```

Alternatively, cumulative format can be turned off using ``prof.str(false)`` then time spent on each
call excluding any children is shown.
```
Profiler "main()"
                           (other): calls=1, wall=2
             perform_calculation(): calls=1, wall=2
perform_calculation():operation1(): calls=2, wall=4
perform_calculation():operation2(): calls=2, wall=4
                      initialise(): calls=1, wall=2
         initialise():read_input(): calls=1, wall=2
     initialise():restore_backup(): calls=1, wall=2
```

# Weak Singleton pattern
During development and debugging it becomes inconvenient to pass a profiler instance down through all function calls.
Instead, one can use the weak Singleton interface to access a profiler that was created at higher scope. 
For example,
  
```cpp
// File: main.cpp
#include <molpro/Profiler.h>
void run();
int main(){
    std::shared_ptr<Profiler> prof = molpro::Profiler::single("MainProfiler");
    run();
    // ...
    return 0;
}
```
```cpp
// File: run.cpp
#include <molpro/Profiler.h>
void run(){
    auto p = molpro::Profiler::single()->push("run()");
    // ...
}
```

The main routine creates a profiler instance that is registered as a static weak_ptr and can be accessed
at lower levels through **molpro::Profiler::single()**.
This pattern avoids the pitfalls of the traditional Singleton by keeping Profiler instance on the heap
with scoped memory management and only storing a non-owning pointer on the stack.

# Profiling and Reporting

Profiling is performed by `molpro::profiler::Profiler` class, there is an alias to it in `molpro/Profiler.h`.
The results of Profiler can be reported using `molpro::profiler::report()` functions in `molpro/profiler/report.h`

# Features
  * Constructs profiler call tree and accumulates statistics for each node
  * Multiple call trees can be processed, allowing simple use in parallel applications
  * management of global Profiler objects using a singleton class
  * includes Fotran and C bindings
  * easily integration into the build system with CMake
  
# CMake build
  Profiler can be added to an existing project that uses CMake by downloading it with FetchContent.
  If Fortran compilers or MPI are enabled during the CMake build than relevant Profiler functionality will be
  build automatically.
  Simply link **molpro::profiler** target to your library.
  
  Profiler uses [DependencyManager](https://github.com/molpro/dependency-manager)
  and hosts its doxygen tag files. 
  You can link your doxygen generated documentation with Profiler using 
  [DependencyManager_Doc](https://molpro.github.io/dependency-manager/dependency_manager_docs.html).

# Contributors

Peter J. Knowles

Marat Sibaev

Iakov Polyak

Rob Welch

# Licence

MIT License
Copyright (c) 2020 Peter James Knowles
