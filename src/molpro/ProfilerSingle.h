#ifndef MOLPRO_PROFILERSINGLE_H
#define MOLPRO_PROFILERSINGLE_H

#include "molpro/Profiler.h"

#include <map>
#include <memory>

namespace molpro {

/*!
 * @brief Access to a single profiler for a given name and communicator if compiled with MPI
 *
 * This is a utility to avoid creation of a global object for a profiler that is used through out the application.
 *
 * For local profilers which might be used only within a particular class composition should still be preferred
 * instead of creating a global instance.
 * @todo Merge create and instance into one function
 * @todo add set_default(Profiler&) and set_default(name, key) functions
 */
class ProfilerSingle {
public:
    ProfilerSingle() = delete;
    using key_t = std::pair<std::string, size_t>;
    using profilers_t = std::map<key_t, std::shared_ptr<Profiler>>;

public:

    /*!
     * @brief Creates an instance of a profiler identified by its name and communicator if compiled with mpi
     *
     * @param name passed to Profiler() constructor
     * @param sortBy passed to Profiler() constructor
     * @param level passed to Profiler() constructor
     * @param communicator name of communicator, if not using MPI than it is irrelevant, passed to Profiler() constructor
     * @param set_default sets this profiler as default, allows access with ProfilerSingle::instance() without arguments
     * @param replace if a profiler with the same key already exists replace it with a new on
     * @return the new profiler instance
     */
    static std::shared_ptr<Profiler>
    create(const std::string &name, Profiler::sortMethod sortBy = Profiler::wall, int level = INT_MAX,
           Profiler::key_t communicator = PROFILER_DEFAULT_KEY, bool set_default = true, bool replace = false) {
        auto key = _key(name, communicator);
        if (replace || m_profilers.count(key) == 0)
            m_profilers[key] = std::make_shared<Profiler>(name, sortBy, level, communicator);
        if (set_default)
            default_key = key;
        return m_profilers[key];
    }

    static std::shared_ptr<Profiler>
    create(const std::string &name, Profiler::key_t communicator, bool set_default, bool replace) {
        return create(name, Profiler::wall, INT_MAX, communicator, set_default, replace);
    }

    static std::shared_ptr<Profiler> create(const std::string &name, bool set_default, bool replace) {
        return create(name, Profiler::wall, INT_MAX, PROFILER_DEFAULT_KEY, set_default, replace);
    }

    /*!
     * @brief Returns a global profiler instance created by ProfilerSingle::create()
     * @param name  name of the Profiler
     * @param communicator mpi communicator
     * @param set_default sets this profiler as default, allows access with ProfilerSingle::instance() without arguments
     */
    static std::shared_ptr<Profiler>
    instance(const std::string &name, Profiler::key_t communicator = PROFILER_DEFAULT_KEY, bool set_default = false) {
        auto key = _key(name, communicator);
        if (set_default)
            default_key = key;
        return instance(key);
    }

    //! Return the default global profiler
    static std::shared_ptr<Profiler>
    instance() {return instance(default_key);}

    /*!
     * @brief Destroys a global instance
     * @param name  name of the Profiler
     * @param communicator mpi communicator
     */
    static void
    destroy(const std::string &name, Profiler::key_t communicator = PROFILER_DEFAULT_KEY) {
        auto key = key_t{name, communicator};
        m_profilers.erase(key);
    }

    //! access profiler objects
    static const profilers_t &
    profilers() {return m_profilers;}

private:
    // Profiler::key_t is turned into a hash, so that multiple keys can be handled without recompilation
    static key_t
    _key(const std::string &name, const Profiler::key_t &key) {return {name, std::hash<Profiler::key_t>{}(key)};}

    static std::shared_ptr<Profiler>
    instance(const key_t &key) {return m_profilers.at(key);}


    /*!
     * @brief collection of global m_profilers
     *
     * They are made public to allow finer control, with the hope that this trust will not be abused.
     */
    static profilers_t m_profilers;
    static key_t default_key;
};

} // namespace molpro

#endif //MOLPRO_PROFILERSINGLE_H
