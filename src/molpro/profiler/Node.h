#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_NODE_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_NODE_H
#include <list>
#include <map>
#include <memory>
#include <stdexcept>
#include <string>

namespace molpro {
namespace profiler {

struct NodePathError : std::runtime_error {
  using std::runtime_error::runtime_error;
};

/*!
 * @brief A node in a parameter tree storing a Counter object aliased by a name
 */
template <class Counter>
class Node {
private:
  explicit Node(std::string name, Counter counter, std::shared_ptr<Node<Counter>> parent)
      : name(std::move(name)), counter(std::move(counter)), parent(std::move(parent)) {}

public:
  ~Node() = default;
  Node(Node<Counter>&&) noexcept = default;
  Node<Counter>& operator=(Node<Counter>&&) noexcept = default;

  Node(const Node<Counter>&) = delete;
  Node<Counter>& operator=(const Node<Counter>&) = delete;

  //! Builds the root tree whose parent is nullptr
  static std::shared_ptr<Node<Counter>> make_root(const std::string& name, const Counter& counter) {
    return std::make_shared<Node<Counter>>(Node<Counter>(name, counter, nullptr));
  }

  //! Adds a child to the current node. Overwrites any children with child_name that already exist.
  static std::shared_ptr<Node<Counter>> add_child(const std::string& child_name, const Counter& child_counter,
                                                  const std::shared_ptr<Node<Counter>>& parent) {
    auto child = std::make_shared<Node<Counter>>(Node<Counter>(child_name, child_counter, parent));
    parent->children[child_name] = child;
    return child;
  }

  //! Creates a deep copy of the subtree with copies of Counter objects.
  static std::shared_ptr<Node<Counter>> deep_copy(const std::shared_ptr<Node<Counter>>& subtree,
                                                  std::shared_ptr<Node<Counter>> parent) {
    auto tree_copy = make_root(subtree->name, subtree->counter);
    tree_copy->parent = std::move(parent);
    for (auto& child : subtree->children)
      tree_copy->children[child.first] = deep_copy(child.second, tree_copy);
    return tree_copy;
  }

  /*!
   * @brief Recursively walks down a single branch described by node names
   *
   * Consider a single branch root->A->B->C, the path can be stored as `std::list<std::string> path{"A","B","C"}`
   * and calling `root.walk(path.begin, path.end)` returns the node C.
   *
   * @tparam ForwardIt forward iterator for a container of strings
   * @param start_name name for a child of the current node
   * @param end_name indicates end of the path
   * @return
   */
  template <typename ForwardIt>
  std::shared_ptr<Node<Counter>> walk(ForwardIt start_name, ForwardIt end_name) {
    auto it = children.find(*start_name);
    if (it == end(children))
      throw NodePathError(std::string("Child was not found. Path is invalid."));
    if (++start_name == end_name)
      return it->second;
    else
      return it->second->walk(start_name, end_name);
  }

  std::shared_ptr<Node<Counter>> walk(const std::list<std::string>& path_to_node) {
    return walk(begin(path_to_node), end(path_to_node));
  }

  //! Get total number of nodes in a tree
  size_t count_nodes() const {
    size_t n = 1;
    for (auto& child : children)
      n += child.second->count_nodes();
    return n;
  }

  std::string name; //!< name of the node. This is a duplicate, same name is stored in parent's map of children.
  Counter counter;  //! resource counter
  std::shared_ptr<Node<Counter>> parent = nullptr;                //!< parent node
  std::map<std::string, std::shared_ptr<Node<Counter>>> children; //!< child nodes
};

} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_NODE_H
