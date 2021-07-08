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

enum EntryType{ node, edge };

/*! Represents a node (box) or edge in a graphviz digraph. */
class GraphEntry{

  public:

    EntryType entry_type; // whether the entry is a node or edge
    std::string name; // name of the node (used for labels and connectivity)
    double runtime; // runtime of the node/edge
    int calls; // number of calls for the node/edge
    std::string name_to; // if this is a edge, this is where it goes to
    std::string fontcolour; // colour of the font for an edge
    int operations; // number of operations in a node
    
    GraphEntry(EntryType entry_type, std::string name, double runtime, int calls,
            double total_time, int operations = -1, std::string name_to = "" );

    std::pair<std::string, std::string> get_colours(int hot[3], int cool[3], double total_time);

};

// This file contains functions related to the creation of graphviz .dot files from profiles. The interface can be found
// in profiler::dotgraph.

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
   * @brief Combines two graph entries, summing their time, calls and opcount.
   * @param entry1 the graph entry that entry2 will be merged into.
   * @param entry2 the graph entry that will be merged into entry1.
   * @return nothing, modifies entry1. If you use this function, erase entry2 afterward.
   */
void combine_graph_entries(GraphEntry& entry1, GraphEntry& entry2);

  /*!
   * @brief Iterates through the list of GraphEntries and merges any two nodes with the same name.
   * The implementation is a little complex, it removes elements from the vector as it goes and has to update the
   * iterators to correct for entries being deleted.
   * @param graph_entries a vector filled with GraphEntry objects, normally found in make_dotgraph_vec
   * @return nothing, modifies graph_entries.
   */
void merge_vec(std::vector<GraphEntry>& graph_entries);

  /*!
   * @brief This removes any enntry from graph_entries with a runtime/total_time less than threshold.
   * @param graph_entries a vector filled with GraphEntry objects.
   * @param threshold the threshold below which the entry will be deleted (as a ratio)
   * @param total_time total runntime of the profile.
   * @return nothing, but modifies graph_entries.
   */
void apply_threshold(std::vector<GraphEntry>& graph_entries, double threshold, double total_time);

  /*!
   * @brief Determines if a node has a parent.
   * @param child - a single graph_entry
   * @param graph_entries a vector filled with GraphEntry objects.
   * @return true if child has a parent in graph_entries, false if it does not.
   */
bool has_parent(GraphEntry& child, std::vector<GraphEntry>& graph_entries);

  /*!
   * @brief Removes every element from graph_entries that does not have an edge pointing towards it (unless it's the
   * root node).
   * @param graph_entries a vector filled with GraphEntry objects, normally found in make_dotgraph_vec
   * @return nothing, modifies graph_entries.
   */
void destroy_orphans(std::vector<GraphEntry>& graph_entries);

  /*!
   * @brief This gets the graphviz markup for the main part of the graph (excepting global styles).
   * @param graph_entries a vector filled with GraphEntry objects.
   * @param total_time the total time taken by the program, in seconds.
   * @param hot an 8-bit colour, 0-255. The more time spent in this node, the hotter the colour will be.
   * @param cool an 8-bit colour, 0-255.
   * @return a string containing graphviz markup for nodes and edges.
   */
std::string get_graph_markup(std::vector<GraphEntry>& graph_entries, double total_time, int hot[3], int cool[3]);

  /*!
   * @brief This populates a vector containing a GraphEntry for each profiler node. This is an intermediate data
   * structure to make it easier to merge profiler nodes but retain their connectivity.
   * @param root the root of the tree of nodes as defined in node.h.
   * @param total_time the total time taken by the program, in seconds.
   * @param threshold a value between 0 and 1, a ratio of the program's runtime. If the program spends less than this
   * value in a given function, the box will not be drawn.
   * @param graph_entries a vector (most likely empty) that will be filled with GraphEntry objects.
   * @return nothing, but populates graph_entries.
   */
void make_dotgraph_vec(std::shared_ptr<Node<Counter>> root, double total_time,
                /*out*/ std::vector<GraphEntry>& graph_entries);

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