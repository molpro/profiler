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
    static std::shared_ptr<Profiler>
    Instance(const std::string &name = "", Profiler::sortMethod sortBy = Profiler::wall, const int level = INT_MAX,
             const MPI_Comm communicator = MPI_COMM_WORLD //< * The MPI communicator over which statistics should be aggregated.
    ) {
        auto key = std::make_pair(name, communicator);
        if (m_profiler.count(key) == 0)
            m_profiler.insert({key, std::make_shared<Profiler>(name, sortBy, level, communicator)});
        return m_profiler.at(key);
    }
#else
    static std::shared_ptr<Profiler>
    Instance(const std::string &name = "", Profiler::sortMethod sortBy = Profiler::wall, const int level = INT_MAX) {
        if (!m_profiler.count(name))
            m_profiler.insert({name, std::make_shared<Profiler>(name, sortBy, level)});
        return m_profiler.at(name);
    }
#endif

protected:
#ifdef PROFILER_MPI
    static std::map<std::pair<std::string, MPI_Comm>, std::shared_ptr<Profiler>> m_profiler;
#else
    static std::map<std::string, std::shared_ptr<Profiler>> m_profiler;
#endif

};


#endif //PROFILER_PROFILERSINGLE_H
