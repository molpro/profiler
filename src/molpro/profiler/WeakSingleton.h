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
    auto& reg = registry();
    auto it = std::find_if(begin(reg), end(reg), [&key](const key_t& el) { return std::get<0>(el) == key; });
    if (it != reg.end())
      result = std::get<1>(*it).lock();
    if (!result) {
      result = std::make_shared<Object>(std::forward<T>(constructor_args)...);
      reg.emplace_back(key_t{key, result, result.get()});
    }
    return result;
  }

  //! Access the last registered object
  static std::shared_ptr<Object> single() {
    auto& reg = registry();
    if (reg.empty() or not std::get<1>(reg.back()).lock()) { // default zero-depth instance
      auto result = Profiler::single("default");
      result->set_max_depth(0);
      // It is our job to keep the default instance alive by always retaining a shared_ptr
      // to it. This way, callers don't have to manage the default instance's lifetime.
      default_instance_saver() = result;
      return result;
    }
    assert(!reg.empty() && "First must make a call to single(key, ...) to create an object");
    std::shared_ptr<Object> result = std::get<1>(reg.back()).lock();
    assert(result && "The last registered object was deallocated");
    return result;
  }

  //! Remove object from the register. This should be called in the destructor of class that exposes this pattern
  static void erase(Object* obj) {
    auto& reg = registry();
    auto it = std::find_if(begin(reg), end(reg), [obj](const key_t& el) { return std::get<2>(el) == obj; });
    if (it != reg.end())
      reg.erase(it);
  }

  //! Remove object registered under the name key.
  static void erase(const std::string& key) {
    auto& reg = registry();
    auto it = std::find_if(begin(reg), end(reg), [&key](const key_t& el) { return std::get<0>(el) == key; });
    if (it != reg.end())
      reg.erase(it);
  }

  //! Remove all registered objects
  static void clear() { registry().clear(); }

  //! Stores all objects created by a call to single(). A function-local static (construct-on-first-use)
  //! so that its destruction order relative to default_instance_saver() is well-defined: see single().
  static std::list<key_t>& registry() {
    static std::list<key_t> reg;
    return reg;
  }

  //! Keeps the zero-depth "default" instance created by single() alive; see the comment there.
  static std::shared_ptr<Object>& default_instance_saver() {
    static std::shared_ptr<Object> saver;
    return saver;
  }
};

} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_SINGLE_H
