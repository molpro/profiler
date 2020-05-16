#ifndef PROFILER_PROFILER_H
#define PROFILER_PROFILER_H
#ifdef PROFILER_MPI
#include "molpro/Profiler/ProfilerMPI.h"
#define PROFILER_PROFILER_H_CLASS ProfilerMPI
#else
#include "molpro/Profiler/ProfilerSerial.h"
#define PROFILER_PROFILER_H_CLASS ProfilerSerial
#endif

namespace molpro {
struct Profiler : public PROFILER_PROFILER_H_CLASS {
using PROFILER_PROFILER_H_CLASS::PROFILER_PROFILER_H_CLASS;
};

} // namespace molpro

#undef PROFILER_PROFILER_H_CLASS
#endif //PROFILER_PROFILER_H
