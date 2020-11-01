#ifndef MOLPRO_PROFILERSINGLE_H
#define MOLPRO_PROFILERSINGLE_H

#include "molpro/Profiler.h"

#include <memory>

namespace molpro {

/*!
 * @brief A non-owning interface to Profiler
 *
 * This is a utility that helps avoid the need to pass an instance of Profiler through the call stack by
 * keeping a weak_ptr to all created objects. At least one of the objects returned by create() must be alive,
 * otherwise ProfilerSingle will return a nullptr.
 *
 * The primary purpose is for quick debugging with minimal changes. In production code the Profiler object
 * should be passed around directly.
 *
 */
class ProfilerSingle {
public:
  ProfilerSingle() = delete;
  using key_t = std::pair<std::string, size_t>;
  using profilers_t = std::map<key_t, std::weak_ptr<Profiler>>;

  /*!
   * @brief Creates an instance of a profiler identified by its name and communicator if compiled with mpi
   *
   * @param name passed to Profiler() constructor
   * @param sortBy passed to Profiler() constructor
   * @param level passed to Profiler() constructor
   * @param communicator name of communicator, if not using MPI than it is irrelevant, passed to Profiler() constructor
   * \param cpu Whether to poll CPU time
   * @param set_default sets this profiler as default, allows access with ProfilerSingle::instance() without arguments
   * @param replace if a profiler with the same key already exists replace it with a new on
   * @return the new profiler instance
   */
  static std::shared_ptr<Profiler> create(const std::string &name, Profiler::sortMethod sortBy = Profiler::wall,
                                          int level = INT_MAX, Profiler::key_t communicator = PROFILER_DEFAULT_KEY,
                                          bool cpu = false, bool set_default = true, bool replace = false) {
    auto key = _key(name, communicator);
    auto p = (replace || m_profilers.count(key) == 0)
                 ? std::make_shared<Profiler>(name, sortBy, level, communicator, cpu)
                 : m_profilers[key].lock();
    m_profilers[key] = p;
    if (set_default)
      default_key = key;
    return p;
  }

  static std::shared_ptr<Profiler> create(const std::string &name, Profiler::key_t communicator, bool cpu,
                                          bool set_default, bool replace) {
    return create(name, Profiler::wall, INT_MAX, communicator, cpu, set_default, replace);
  }

  static std::shared_ptr<Profiler> create(const std::string &name, bool cpu, bool set_default, bool replace) {
    return create(name, Profiler::wall, INT_MAX, PROFILER_DEFAULT_KEY, cpu, set_default, replace);
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
  static std::shared_ptr<Profiler> instance() { return instance(default_key); }

  //! access profiler objects
  static const profilers_t &profilers() { return m_profilers; }

private:
  // Profiler::key_t is turned into a hash, so that multiple keys can be handled without recompilation
  static key_t _key(const std::string &name, const Profiler::key_t &key) {
    return {name, std::hash<Profiler::key_t>{}(key)};
  }

  static std::shared_ptr<Profiler> instance(const key_t &key) { return m_profilers.at(key).lock(); }

  /*!
   * @brief collection of global m_profilers
   *
   * They are made public to allow finer control, with the hope that this trust will not be abused.
   */
  static profilers_t m_profilers;
  static key_t default_key;
};

} // namespace molpro

#endif // MOLPRO_PROFILERSINGLE_H
