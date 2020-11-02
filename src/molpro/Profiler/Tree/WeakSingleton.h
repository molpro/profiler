#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
#include <algorithm>
#include <cassert>
#include <list>
#include <memory>
#include <string>

namespace molpro {
namespace profiler {
namespace tree {

// FIXME improve description
/*!
 * @brief Implements the mechanism for the weak singleton pattern
 */
template <class Object>
struct WeakSingleton {
  /*!
   * @brief Creates an instance of Object or returns an already registered instance
   * @param constructor_args arguments that should be passed to the constructor
   */
  template <typename... T>
  static std::shared_ptr<Object> single(const std::string& key, T&&... constructor_args) {
    std::shared_ptr<Object> result = nullptr;
    auto it = std::find_if(begin(m_register), end(m_register),
                           [&key](const std::pair<std::string, std::weak_ptr<Object>>& el) { return el.first == key; });
    if (it != m_register.end())
      result = it->second.lock();
    if (!result) {
      result = std::make_shared<Object>(std::forward<T>(constructor_args)...);
      m_register.emplace_back(std::pair<std::string, std::weak_ptr<Object>>{key, result});
    }
    return result;
  }

  //! Access the last registered object
  static std::shared_ptr<Object> single() {
    assert(!m_register.empty() && "First must make a call to single(key, ...) to create an object");
    std::shared_ptr<Object> result = m_register.front().second.lock();
    assert(result && "The last registered object was deallocated");
    return result;
  }

  static std::list<std::pair<std::string, std::weak_ptr<Object>>>
      m_register; //!< stores all objects created by a call to single
};

} // namespace tree
} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
