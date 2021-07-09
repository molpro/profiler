#ifndef PROFILER_SRC_MOLPRO_PROFILER_PROFILER_H
#define PROFILER_SRC_MOLPRO_PROFILER_PROFILER_H
#ifdef MOLPRO_PROFILER_MPI
#include <mpi.h>
#endif

#include <molpro/profiler/SortBy.h>

#include <memory>
#include <ostream>
#include <string>
#include <fstream>
#include <vector>
#include <stdexcept>

namespace molpro {
namespace profiler {

static int hot_default[3] = {255,0,0};
static int cool_default[3] = {0,0,255};
static std::vector<std::pair<double,double>> default_heat_adjust = {{}};

class Counter;
template <class CounterT>
class Node;

/*!
 * @brief Instrumental profiler for timing sections of code
 *
 * Profiler constructs and navigates the call tree using start() and stop() to move between nodes or create them on
 * first run.
 * The profiler uses wall clock by default, but could also use cpu time, or do no timing at all and simply
 * accumulate number of calls to each node.
 * The maximum depth of profiler tree can be set using set_max_depth(), any nodes below max_depth are not created.
 * This allow for profiler calls to be used in Production code without degrading the performance
 *
 * @note See README.md and examples for different ways to use the Profiler, and see report() on how the results can we
 * reported.
 *
 */
class Profiler {
protected:
  std::string m_description;       //!< name of the root node
  std::string m_root_name = "All"; //!< name of the root node
  int m_max_depth;         //!< max depth level of profiler tree counting root as 0. Defaults to largest possible value.
  int m_current_depth = 0; //!< current depth of the active node
public:
  std::shared_ptr<profiler::Node<profiler::Counter>> root;        //!< root node of the profiler call tree
  std::shared_ptr<profiler::Node<profiler::Counter>> active_node; //!< the most recent active node.
  /*!
   * @brief Construct profiler and start timing
   * @param description_ description of profiler
   * @param with_wall whether to include wall time
   * @param with_cpu whether to include cpu time
   */
  explicit Profiler(std::string description_, bool with_wall = true, bool with_cpu = false);
  Profiler(Profiler&&) = default;
  Profiler& operator=(Profiler&&) = default;
  ~Profiler();

  Profiler() = delete;
  Profiler(const Profiler&) = delete;
  Profiler& operator=(const Profiler&) = delete;

  const std::string& description() const { return m_description; }

  //! Access a Profiler registered with the WeakSingleton creating a new one if a Profiler instance with that
  //! description does not exist
  static std::shared_ptr<Profiler> single(const std::string& description_, bool with_wall = true,
                                          bool with_cpu = false);
  //! Access the last registered Profiler
  static std::shared_ptr<Profiler> single();
  //! Remove Profiler with specified description from the singleton register
  static void erase(const std::string& description);

  /*!
   * @brief Get the maximum depth the profiler tree is allowed to reach
   *
   * Any calls to start() that would lead to profiler tree growing above max depth do nothing.
   * The corresponding stop() still needs to be posted, since the virtual depth of the tree
   * is still being tracked.
   *
   */
  int get_max_depth() const;
  //! Set the maximum depth the profiler tree is allowed to reach
  void set_max_depth(int new_max_depth);
  //! Get the current depth of the call stack from root to the active node. This tracks virtual nodes beyond max_depth,
  //! even though they are not constructed.
  int get_current_depth() const;

  /*!
   * @brief Traverse down to a child node and start timing
   * @param name name of the child node
   * @return
   */
  Profiler& start(const std::string& name);

  //! Stop the active node and traverse up to its parent
  Profiler& stop(const std::string& name="");

  //! Stop all nodes and traverse up to the root
  Profiler& stop_all();

  //! Access counter at the top of the call stack
  profiler::Counter& counter();

  // FIXME Why is this even needed?
  //! Erases all data and starts from root again
  Profiler& reset(const std::string& name);

  /*!
   * \brief Advance the counter holding the notional number of operations executed in the code segment.
   * \param operations The number of additional operations.
   */
  void operator+=(size_t operations);

  /*!
   * \brief Advance the counter holding the notional number of operations executed in the code segment.
   */
  void operator++();

  size_t operator++(int);

protected:
  //! Proxy object that calls start() on creation and stop() on destruction
  struct Proxy {
    Profiler& prof; //!< reference to the underlying profiler

    Proxy(Profiler& prof, const std::string& name) : prof(prof) { prof.start(name); }
    ~Proxy() { prof.stop(); }

    //! Push a new proxy
    Proxy push(const std::string& name) { return Proxy(prof, name); }

    /*!
     * \brief Advance the counter holding the notional number of operations executed in the code segment.
     * \param operations The number of additional operations.
     */
    void operator+=(size_t operations);

    /*!
     * \brief Advance the counter holding the notional number of operations executed in the code segment.
     */
    void operator++();

    size_t operator++(int);
  };

public:
  //! Returns a proxy object which will start() on construction and stop on destruction.
  Proxy push(const std::string& name) { return Proxy(*this, name); }

  //! Produce a report on profiler as a string
  //! @param cumulative whether to use cumulative times or subtract the time spend by children
  std::string str(bool cumulative = true, profiler::SortBy sort_by = profiler::SortBy::wall) const;

  /*!
   * @brief Get a graphviz .dot markup file for a profile.
   * @param path path to the file to be written to disk. This should be an absolute path, as the working directory
   * may be a temporary folder.
   * @param threshold - ratio of the time spent in one node to the whole runtime. If the time spent in that node is
   * less that the threshold, the node is hidden.
   * @param hot the RGB values (0-255) of the 'hot' colour (a ratio of 1 will produce the hot colour, 0 the cool)
   * @param cool the RGB values (0-255) of the 'cool' colour.
   * @param cumulative whether to print the cumulative time, or to subtract the time spent in the children from a node's
   * timings.
   * @param sort_by whether to sort by wall clock time, calls, frequency, etc. Currently unused!
   * @param heat_adjust currently unused, will eventually be used to establish non-linear mappings between ratio and
   * colour.
   * @param get_percentage_time whether to show the time as a percentage of the runtime (true) or in seconds (false).
   * @return a string containing the dotgraph. Using this string is optional as the dotgraph is already written to path.
   */
  std::string dotgraph(std::string path, double threshold = 0.01, bool cumulative=true, int hot[3] = hot_default,
                      int cool[3] = cool_default, SortBy sort_by = profiler::SortBy::none,
                      std::vector<std::pair<double,double>> heat_adjust = default_heat_adjust,
                      bool get_percentage_time = false);

#ifdef MOLPRO_PROFILER_MPI
  std::string str(MPI_Comm communicator, bool cumulative = true,
                  profiler::SortBy sort_by = profiler::SortBy::wall) const;

  /*!
   * @brief Get a graphviz .dot markup file for a profile.
   * @param path path to the file to be written to disk. This should be an absolute path, as the working directory
   * may be a temporary folder.
   * @param communicator instance of MPI_Comm
   * @param root_process - determines whether reduce_root_only or reduce_all is run when synchronising the tree.
   * @param threshold - ratio of the time spent in one node to the whole runtime. If the time spent in that node is
   * less that the threshold, the node is hidden.
   * @param hot the RGB values (0-255) of the 'hot' colour (a ratio of 1 will produce the hot colour, 0 the cool)
   * @param cool the RGB values (0-255) of the 'cool' colour.
   * @param cumulative whether to print the cumulative time, or to subtract the time spent in the children from a node's
   * timings.
   * @param sort_by whether to sort by wall clock time, calls, frequency, etc. Currently unused!
   * @param heat_adjust currently unused, will eventually be used to establish non-linear mappings between ratio and
   * colour.
   * @param get_percentage_time whether to show the time as a percentage of the runtime (true) or in seconds (false).
   * @return a string containing the dotgraph. Using this string is optional as the dotgraph is already written to path.
   */
  std::string dotgraph(std::string path, MPI_Comm communicator, int root_process, double threshold = 0.01,
                      int hot[3] = hot_default, int cool[3] = cool_default, bool cumulative=true,
                      SortBy sort_by = profiler::SortBy::none,
                      std::vector<std::pair<double,double>> heat_adjust = default_heat_adjust,
                      bool get_percentage_time = false);
#endif

  friend std::ostream& operator<<(std::ostream& os, const Profiler& obj);
};

} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_PROFILER_H
