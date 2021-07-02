#ifndef PROFILER_SRC_MOLPRO_PROFILER_DOTGRAPH
#define PROFILER_SRC_MOLPRO_PROFILER_DOTGRAPH
#include <sstream>
#include <string>
#include <iomanip>
#include <cmath>

#include <molpro/profiler/Counter.h>
#include <molpro/profiler/Node.h>
#include <molpro/profiler/report.h>

namespace molpro {
namespace profiler {
namespace dotgraph {

// This file contains functions related to the creation of graphviz .dot files from profiles. The interface can be found
// in profiler::dotgraph.

  /*!
   * @brief Get the frequency of an operation as a string with units of Hz.
   * @param n_op the number of operations as counted by the profiler (e.g counter.get_call_count())
   * @param time time taken in seconds.
   * @return the frequency, as a std::string, with units of Hz.
   */
std::string frequency(size_t n_op, double time);

  /*!
   * @brief Simple additive blending of two colours. May be reduced with nicer colour blending in the future.
   * @param ratio ratio of the colours. A double between 0 and 1. 1 is hot, 0 is cool.
   * @param hot_colour an 8-bit colour, 0-255.
   * @param cool_colour an 8-bit colour, 0-255.
   * @return the colour, in hex format, as a string, e.g. #fffeee
   */
std::string blend_colours(double ratio, int hot_colour[3], int cool_colour[3]);

  /*!
   * @brief Create a graphviz box of a profiler node.
   * @param ratio name the name of the box.
   * @param time time time taken for the node, in seconds.
   * @param total_time the total time taken by the program, in seconds.
   * @param call_count the number of times the node has been called.
   * @param opcount the number of operations (used to calculate frequency)
   * @param hot an 8-bit colour, 0-255. The more time spent in this node, the hotter the colour will be.
   * @param cool an 8-bit colour, 0-255.
   * @return the box, as graphviz markup.
   */
std::string make_box(std::string name, double time, double total_time, size_t call_count, size_t opcount,
                      int hot[3], int cool[3]);

  /*!
   * @brief Create a graphviz arrow of a profiler node.
   * @param name_from name the name of the box that the arrow is coming from.
   * @param ratio name the name of the box that the arrow is going to.
   * @param time time time taken for the node, in seconds.
   * @param total_time the total time taken by the program, in seconds.
   * @param call_count the number of times the node has been called.
   * @param hot an 8-bit colour, 0-255. The more time spent in this node, the hotter the colour will be.
   * @param cool an 8-bit colour, 0-255.
   * @return the arrow, as graphviz markup.
   */
std::string make_arrow(std::string name_from, std::string name_to, double time, double total_time, size_t call_count,
                        int hot[3], int cool[3]);

  /*!
   * @brief This creates graphviz markup for all of the arrows and boxes, but not global styles.
   * @param root the root of the tree of nodes as defined in node.h.
   * @param total_time the total time taken by the program, in seconds.
   * @param hot an 8-bit colour, 0-255. The more time spent in this node, the hotter the colour will be.
   * @param cool an 8-bit colour, 0-255.
   * @param threshold a value between 0 and 1, a ratio of the program's runtime. If the program spends less than this
   * value in a given function, the box will not be drawn.
   * @return graphviz markup for the arrows and boxes.
   */
std::string make_dotgraph_contents(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                                    double threshold);

  /*!
   * @brief This creates the complete graphviz markup for the whole performance graph, including global styles. The
   output from this function can be written to a .dot file.
   * @param root the root of the tree of nodes as defined in node.h.
   * @param total_time the total time taken by the program, in seconds.
   * @param hot an 8-bit colour, 0-255. The more time spent in this node, the hotter the colour will be.
   * @param cool an 8-bit colour, 0-255.
   * @param threshold a value between 0 and 1, a ratio of the program's runtime. If the program spends less than this
   * value in a given function, the box will not be drawn.
   * @return graphviz markup for the profile.
   */
std::string make_dotgraph(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                            double threshold);

} // namespace dotgraph
} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_DOTGRAPH