cmake_minimum_required(VERSION 2.8)

project(Profiler)
ENABLE_LANGUAGE(Fortran)

file (GLOB SRCS *.cpp *.h)
add_executable(Profiler.exe main.cpp Profiler.cpp Profiler.h ProfilerC.h)
add_executable(ProfilerF.exe mainF.F90 Profiler.cpp Profiler.h ProfilerC.h ProfilerF.F90)


