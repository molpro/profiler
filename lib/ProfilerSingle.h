#ifndef PROFILER_PROFILERSINGLE_H
#define PROFILER_PROFILERSINGLE_H

#include "Profiler.h"

#include <map>
#include <memory>


/*!
 * @brief Access to a single profiler for a given name and communicator if compiled with MPI
 *
 * This is a utility to avoid creation of a global object for a profiler that is used through out the application.
 *
 * For local profilers which might be used only within a particular class composition should still be preferred
 * instead of creating a global instance.
 */
class ProfilerSingle {
public:
    ProfilerSingle() = delete;
#ifdef PROFILER_MPI
    using key_t = std::pair<std::string, MPI_Comm>;
#else
    using key_t = std::string;
#endif
    using profilers_t = std::map<key_t, std::shared_ptr<Profiler>>;


    /*!
     * @brief Creates an instance of a profiler identified by its name and communicator if compiled with mpi
     *
     * @param name passed to Profiler() constructor
     * @param sortBy passed to Profiler() constructor
     * @param level passed to Profiler() constructor
     * @param communicator passed to Profiler() constructor
     * @param set_default sets this profiler as default, allows access with ProfilerSingle::instance() without arguments
     * @param replace if a profiler with the same key already exists replace it with a new on
     * @return the new profiler instance
     */
    static std::shared_ptr<Profiler>
    create(const std::string &name,
#ifdef PROFILER_MPI
           MPI_Comm communicator = MPI_COMM_WORLD,
#endif
           Profiler::sortMethod sortBy = Profiler::wall, int level = INT_MAX,
           bool set_default = true, bool replace = false);

    /*!
     * @brief Returns a global profiler instance created by ProfilerSingle::create()
     * @param name  name of the Profiler
     * @param communicator mpi communicator
     * @param set_default sets this profiler as default, allows access with ProfilerSingle::instance() without arguments
     */
    static std::shared_ptr<Profiler>
    instance(const std::string &name,
#ifdef PROFILER_MPI
             MPI_Comm communicator = MPI_COMM_WORLD,
#endif
             bool set_default = false);

    //! Return the default global profiler
    static std::shared_ptr<Profiler>
    instance();

    /*!
     * @brief Destroys a global instance
     * @param name  name of the Profiler
     * @param communicator mpi communicator
     */
    static void
    destroy(const std::string &name
#ifdef PROFILER_MPI
            , MPI_Comm communicator = MPI_COMM_WORLD
#endif
    );

    /*!
     * @brief collection of global profilers
     *
     * They are made public to allow finer control, with the hope that this trust will not be abused.
     */
    static profilers_t profilers;
    static key_t default_key;

};


#endif //PROFILER_PROFILERSINGLE_H
