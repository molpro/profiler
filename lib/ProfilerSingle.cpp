#include "ProfilerSingle.h"

#ifdef PROFILER_MPI
std::map<std::pair<std::string, MPI_Comm>, std::shared_ptr<Profiler>> ProfilerSingle::m_profiler{};
#else
std::map<std::string,std::shared_ptr<Profiler>> ProfilerSingle::m_profiler{};
#endif