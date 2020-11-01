#ifndef PROFILER_SRC_MOLPRO_PROFILER_TREE_NODE_H
#define PROFILER_SRC_MOLPRO_PROFILER_TREE_NODE_H
#include <map>
#include <memory>
#include <string>

namespace molpro {
namespace profiler {
namespace tree {
/*!
 * @brief Represents a counter object as a node in a tree.
 */
template <class Counter>
struct Node {
  explicit Node(std::string name, Counter counter, std::shared_ptr<Node> parent = nullptr)
      : name(std::move(name)), counter(std::move(counter)), parent(std::move(parent)) {}

  std::string name; //!< name of the node. This is a duplicate, same name is stored in parent's map of children.
  Counter counter;  //! resource counter
  std::shared_ptr<Node> parent;                          //!< parent node
  std::map<std::string, std::shared_ptr<Node>> children; //!< child nodes
};
} // namespace tree
} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_TREE_NODE_H
