#ifndef MOLPRO_PROFILER_H
#define MOLPRO_PROFILER_H
#ifdef MOLPRO_PROFILER_MPI
#include "molpro/Profiler/ProfilerMPI.h"
#define MOLPRO_PROFILER_CLASS ProfilerMPI
#else
#include "molpro/Profiler/ProfilerSerial.h"
#define MOLPRO_PROFILER_CLASS ProfilerSerial
#endif

namespace molpro {
struct Profiler : public profiler::MOLPRO_PROFILER_CLASS {
  using MOLPRO_PROFILER_CLASS::MOLPRO_PROFILER_CLASS;
};

} // namespace molpro

#undef MOLPRO_PROFILER_CLASS
#endif // MOLPRO_PROFILER_H
