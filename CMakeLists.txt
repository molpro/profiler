cmake_minimum_required(VERSION 2.8)

project(Profiler)

file (GLOB SRCS *.cpp *.h)
add_executable(Profiler.exe ${SRCS})


