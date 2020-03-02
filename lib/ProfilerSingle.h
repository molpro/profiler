#ifndef PROFILER_PROFILERSINGLE_H
#define PROFILER_PROFILERSINGLE_H

#include "Profiler.h"

#ifdef PROFILER_MPI
#include "mpi.h"
#endif
#include <map>
#include <memory>


/*!
 * @brief Access to a single profiler for a given name and communicator if compiled with MPI
 */
class ProfilerSingle {
public:
    /*!
     * @brief Returns a pointer to the profiler instance.
     *
     * Arguments are forwarded to Profiler() and are only relevant on first call,.
     * If compiled with mpi, each communicator has it's own profiler instance.
     *
     */
#ifdef PROFILER_MPI
    using key_t = std::pair<std::string, MPI_Comm>;
#else
    using key_t = std::string;
#endif
    using profilers_t = std::map<key_t, std::shared_ptr<Profiler>>;

#ifdef PROFILER_MPI

    static std::shared_ptr<Profiler>
    Instance(const std::string &name, Profiler::sortMethod sortBy = Profiler::wall, const int level = INT_MAX,
             const MPI_Comm communicator = MPI_COMM_WORLD //< * The MPI communicator over which statistics should be aggregated.
    ) {
        auto key = key_t{name, communicator};
        if (profilers.count(key) == 0)
            profilers.insert({key, std::make_shared<Profiler>(name, sortBy, level, communicator)});
        return profilers.at(key);
    }

#else
    static std::shared_ptr<Profiler>
    Instance(const std::string &name = "", Profiler::sortMethod sortBy = Profiler::wall, const int level = INT_MAX) {
        if (!profilers.count(name))
            profilers.insert({name, std::make_shared<Profiler>(name, sortBy, level)});
        return profilers.at(name);
    }
#endif

    /*!
     * @brief collection of global profilers
     *
     * They are made public to allow finer control, with the hope that this trust will not be abused.
     */
    static profilers_t profilers;

};


#endif //PROFILER_PROFILERSINGLE_H
