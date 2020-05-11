#include "ProfilerSingle.h"
#ifdef PROFILER_MPI
#include "ProfilerMPIConfig.h"
#endif

namespace molpro {

std::shared_ptr<Profiler>
ProfilerSingle::create(const std::string &name, Profiler::sortMethod sortBy, const int level,
#ifdef PROFILER_MPI
                       MPI_Comm communicator,
#endif
                       bool set_default, bool replace) {
#ifdef PROFILER_MPI
    if (communicator == MPI_COMM_WORLD)
        communicator = PROFILER_DEFAULT_COMMUNICATOR;
    auto key = key_t{name, communicator};
#else
    auto key = key_t{name};
#endif
    if (replace || profilers.count(key) == 0)
#ifdef PROFILER_MPI
        profilers[key] = std::make_shared<Profiler>(name, sortBy, level, communicator);
#else
    profilers[key] = std::make_shared<Profiler>(name, sortBy, level);
#endif
    if (set_default)
        default_key = key;
    return profilers[key];
}

/*!
 * @brief Returns a global profiler instance created by ProfilerSingle::create()
 * @param name  name of the Profiler
 * @param communicator mpi communicator
 * @param set_default sets this profiler as default, allows access with ProfilerSingle::instance() without arguments
 */
std::shared_ptr<Profiler>
ProfilerSingle::instance(const std::string &name,
#ifdef PROFILER_MPI
                         MPI_Comm communicator,
#endif
                         bool set_default) {
#ifdef PROFILER_MPI
    if (communicator == MPI_COMM_WORLD)
        communicator = PROFILER_DEFAULT_COMMUNICATOR;
    auto key = key_t{name, communicator};
#else
    auto key = key_t{name};
#endif
    return profilers.at(key);
}

//! Return the default global profiler
std::shared_ptr<Profiler>
ProfilerSingle::instance() {
    return profilers.at(default_key);
}

void
ProfilerSingle::destroy(const std::string &name
#ifdef PROFILER_MPI
        , MPI_Comm communicator
#endif
) {
#ifdef PROFILER_MPI
    if (communicator == MPI_COMM_WORLD)
        communicator = PROFILER_DEFAULT_COMMUNICATOR;
    auto key = key_t{name, communicator};
#else
    auto key = key_t{name};
#endif
    profilers.erase(key);
}

ProfilerSingle::profilers_t ProfilerSingle::profilers{};
ProfilerSingle::key_t ProfilerSingle::default_key{};

} // namespace molpro
