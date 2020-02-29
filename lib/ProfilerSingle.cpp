#include "ProfilerSingle.h"

#ifdef PROFILER_MPI
std::map<MPI_Comm, std::shared_ptr<Profiler>> ProfilerSingle::m_profiler{};
#else
std::shared_ptr<Profiler> ProfilerSingle::m_profiler{};
#endif
