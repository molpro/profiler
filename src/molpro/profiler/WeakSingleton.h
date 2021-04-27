#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
#include <algorithm>
#include <cassert>
#include <list>
#include <memory>
#include <string>

namespace molpro {
namespace profiler {

// FIXME improve description
/*!
 * @brief Implements the mechanism for the weak singleton pattern
 */
template <class Object>
struct WeakSingleton {

  using key_t = std::tuple<std::string, std::weak_ptr<Object>, Object*>;

  /*!
   * @brief Creates an instance of Object or returns an already registered instance
   * @param constructor_args arguments that should be passed to the constructor
   */
  template <typename... T>
  static std::shared_ptr<Object> single(const std::string& key, T&&... constructor_args) {
    std::shared_ptr<Object> result = nullptr;
    auto it =
        std::find_if(begin(m_register), end(m_register), [&key](const key_t& el) { return std::get<0>(el) == key; });
    if (it != m_register.end())
      result = std::get<1>(*it).lock();
    if (!result) {
      result = std::make_shared<Object>(std::forward<T>(constructor_args)...);
      m_register.emplace_back(key_t{key, result, result.get()});
    }
    return result;
  }

  //! Access the last registered object
  static std::shared_ptr<Object> single() {
    if (m_register.empty() or not std::get<1>(m_register.back()).lock()) { // default zero-depth instance
      auto saver = new std::shared_ptr<Profiler>; *saver = Profiler::single("default");
      (*saver)->set_max_depth(0);
      return *saver;
    }
    assert(!m_register.empty() && "First must make a call to single(key, ...) to create an object");
    std::shared_ptr<Object> result = std::get<1>(m_register.back()).lock();
    assert(result && "The last registered object was deallocated");
    return result;
  }

  //! Remove object from the register. This should be called in the destructor of class that exposes this pattern
  static void erase(Object* obj) {
    auto it =
        std::find_if(begin(m_register), end(m_register), [obj](const key_t& el) { return std::get<2>(el) == obj; });
    if (it != m_register.end())
      m_register.erase(it);
  }

  //! Remove object registered under the name key.
  static void erase(const std::string& key) {
    auto it =
        std::find_if(begin(m_register), end(m_register), [&key](const key_t& el) { return std::get<0>(el) == key; });
    if (it != m_register.end())
      m_register.erase(it);
  }

  //! Remove all registered objects
  static void clear() { m_register.clear(); }

  static std::list<key_t> m_register; //!< stores all objects created by a call to single
};

} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
