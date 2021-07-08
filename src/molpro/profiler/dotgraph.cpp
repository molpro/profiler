// todo:
// why is frequency printing broken?

#include "dotgraph.h"

namespace molpro {
namespace profiler {
namespace dotgraph {

// functions to do with creatinng graphviz output

std::string blend_colours(double ratio, int hot_colour[3], int cool_colour[3]){
  std::stringstream ss;
  int return_colour[3];
  unsigned long rgb;
  // blend colours additively
  for(int i=0; i<3; i++){
    return_colour[i] = hot_colour[i]*ratio + cool_colour[i]*(1.0-ratio);
  }
  // convert to hex
  rgb = (return_colour[0] << 16 | return_colour[1] << 8 | return_colour[2] );
  ss << std::hex << "#" << std::setfill('0') << std::setw(6) << rgb; 
  return ss.str();
}

std::string make_box(std::string name, double time, double total_time, size_t call_count, size_t opcount,
                      int hot[3], int cool[3]){
  std::stringstream ss;
  ss << "\"" << name << "\"" << " [" << "color=\"" << blend_colours(time/total_time, hot, cool) <<
  "\", fontcolor=\"#ffffff\", label=\"" << name << "\\n" << (time/total_time)*100 << "%\\n" << call_count << "x";
  if (opcount > 0){
    ss << "\\n" << molpro::profiler::detail::frequency(opcount, time) << "";
  }
  ss << "\"];\n";
  return ss.str();
}

std::string make_arrow(std::string name_from, std::string name_to, double time, double total_time, size_t call_count,
                        int hot[3], int cool[3]){
  std::stringstream ss;
  ss << "\"" << name_from << "\" -> \"" << name_to << "\" [color=\"" << blend_colours(time/total_time, hot, cool)
  << "\", fontcolor=\"" << blend_colours(time/total_time, hot, cool) << "\", label=\"" << (time/total_time)*100
  << "%\\n" << call_count << "x\"];\n";
  return ss.str();
}

std::string get_graph_markup(std::vector<GraphEntry>& graph_entries, double total_time, int hot[3],
                              int cool[3]){
  std::stringstream ss;
  for(int i = 0; i<graph_entries.size(); i++) {
    if (graph_entries[i].entry_type == node){
      ss << make_box(graph_entries[i].name, graph_entries[i].runtime, total_time, graph_entries[i].calls,
                graph_entries[i].operations, hot, cool);
    }
    if (graph_entries[i].entry_type == edge){
      ss << make_arrow(graph_entries[i].name, graph_entries[i].name_to, graph_entries[i].runtime, total_time,
                        graph_entries[i].calls, hot, cool);
    }
  }
  return ss.str();
}

// funtions that operate on GraphEntries or collections

void combine_graph_entries(GraphEntry& entry1, GraphEntry& entry2){
  if (entry1.entry_type != entry2.entry_type){
    throw std::runtime_error("Cannot combine node and edge");
  }
  entry2.runtime += entry1.runtime;
  entry2.calls += entry1.calls;
  entry2.operations += entry1.operations;
}

void make_dotgraph_vec(std::shared_ptr<Node<Counter>> root, double total_time,
                /*out*/ std::vector<GraphEntry>& graph_entries){
  // get values
  auto opcount = root->counter.get_operation_count();
  auto call_count = root->counter.get_call_count();
  double time = root->counter.get_wall().cumulative_time();
  graph_entries.emplace_back( GraphEntry(node, root->name, time, call_count, total_time, opcount, "") );
  for (auto& child : root->children){
    auto child_node = child.second;
    double child_time = child_node->counter.get_wall().cumulative_time();
    auto child_call_count = child_node->counter.get_call_count();
    // make arrows for all children of this box
      graph_entries.emplace_back(GraphEntry(edge, root->name, child_time, child_call_count, total_time, -1,
                                  child_node->name));
      // recurse into children
      make_dotgraph_vec(child_node, total_time, graph_entries);
  }
}

void merge_vec(std::vector<GraphEntry>& graph_entries){
  std::vector<std::pair<std::string, int>> names;
  int size = graph_entries.size();
  for(int i = 0; i<size; i++) {
    bool name_in_names = false;
    for (int j = 0; j != names.size(); j++){
      if (graph_entries[i].name == names[j].first && graph_entries[i].entry_type == node){
        // combine vectors
        combine_graph_entries(graph_entries[i], graph_entries[names[j].second]);
        graph_entries.erase(graph_entries.begin() + i);
        // correct iteration now that the vector has changed size
        --i;
        size = graph_entries.size();
        name_in_names = true;
      }
    }
    // keep track of which names exist and their indices
    if (!name_in_names && graph_entries[i].entry_type == node){
      names.emplace_back(std::make_pair(graph_entries[i].name, i));
    }
  }
}

void apply_threshold(std::vector<GraphEntry>& graph_entries, double threshold, double total_time){
  int size = graph_entries.size();
  for (int i = 0; i<size; i++){
    double time_ratio = graph_entries[i].runtime/total_time;
    if (time_ratio < threshold){
        graph_entries.erase(graph_entries.begin() + i);
        --i;
        size = graph_entries.size();
    }
  }
}

bool has_parent(GraphEntry& child, std::vector<GraphEntry>& graph_entries){
  std::string parent = child.name;
  bool has_parent = false;
  for (int i = 0; i<graph_entries.size(); i++){
    if (graph_entries[i].entry_type == edge && graph_entries[i].name_to == parent){
      has_parent = true;
      break;
    }
  }
  return has_parent;
}

void destroy_orphans(std::vector<GraphEntry>& graph_entries){
  int size = graph_entries.size();
  for (int i = 0; i<size; i++){
    if (!has_parent(graph_entries[i], graph_entries) && graph_entries[i].name != "All"){
        graph_entries.erase(graph_entries.begin() + i);
        --i;
        size = graph_entries.size();
    }
  }
}

// constructur

GraphEntry::GraphEntry(EntryType entry_type, std::string name, double runtime, int calls,
            double total_time, int operations, std::string name_to) 
            : entry_type(entry_type), name(name), runtime(runtime), calls(calls), name_to(name_to),
            operations(operations)
{ }

// this does everything

std::string make_dotgraph(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                            double threshold){
  std::vector<GraphEntry> graph_entries;
  make_dotgraph_vec(root, total_time, graph_entries); // create data structure
  merge_vec(graph_entries); // merge together calls to the same function with different parents
  apply_threshold(graph_entries, threshold, total_time); // cull nodes that run for less than the threshold ratio
  destroy_orphans(graph_entries); // cull any node with no parents
  std::string graph_contents = get_graph_markup(graph_entries, total_time, hot, cool);
  std::stringstream ss;
  // write header info
  ss << "digraph {\n\n";
  ss << "graph [fontname=Arial, nodesep=0.125, ranksep=0.25, fontsize=10.0];\n";
  ss << "node [fontcolor=white, fontname=Arial, height=0, shape=box, style=filled, width=0];\n";
  ss << "edge [fontname=Arial, labeldistance=4.00, penwidth=4.00];\n";
  ss << "\n";
  ss << graph_contents;
  ss << "\n}";
  return ss.str();
}

} // namespace dotgraph
} // namespace profiler
} // namespace molpro