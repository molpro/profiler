#ifndef PROFILER_PROFILERSINGLE_H
#define PROFILER_PROFILERSINGLE_H

#include "Profiler.h"

#ifdef PROFILER_MPI
#include "mpi.h"
#include <map>
#endif
#include <memory>


/*!
 * @brief Access to a single profiler instance per communicator if compiled with MPI
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
        if (m_profiler.count(communicator) == 0) {
            m_profiler.insert({communicator, std::make_shared<Profiler>(name, sortBy, level, communicator)});
        }
        return m_profiler[communicator];
    }
#else
    static std::shared_ptr<Profiler>
    Instance(const std::string &name="", Profiler::sortMethod sortBy = Profiler::wall, const int level = INT_MAX) {
        if (!m_profiler)
            m_profiler = std::make_shared<Profiler>(name, sortBy, level);
        return m_profiler;
    }
#endif

protected:
#ifdef PROFILER_MPI
    static std::map<MPI_Comm, std::shared_ptr<Profiler>> m_profiler;
#else
    static std::shared_ptr<Profiler> m_profiler;
#endif

};


#endif //PROFILER_PROFILERSINGLE_H
