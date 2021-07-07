#include "dotgraph.h"

namespace molpro {
namespace profiler {
namespace dotgraph {


// todo: move this to report.cpp and tidy up write_timing (of which this is one part)
std::string frequency(size_t n_op, double time){
  std::stringstream ss;
  const std::string prefixes{"yzafpnum kMGTPEZY"};
  const int prefix_base = prefixes.find(" ");
  auto rate = double(n_op) / time;
  int prefix_rate = std::min(prefixes.size() - 1, size_t(std::max(0.0, (std::log10(rate) / 3) + prefix_base)));
  ss << " (" << rate / std::pow(1e3, (prefix_rate - prefix_base)) << " ";
  if (prefix_rate != prefix_base)
    ss << prefixes[prefix_rate];
  ss << "Hz)";
  return ss.str();
}

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
    ss << "\\n" << frequency(opcount, time) << "";
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

void combine_graph_entries(GraphEntry& entry1, GraphEntry& entry2){
  if (entry1.entry_type != entry2.entry_type){
    throw std::runtime_error("Cannot combine node and edge");
  }
  entry2.runtime += entry1.runtime;
  entry2.calls += entry1.calls;
  entry2.operations += entry1.operations;
}

void make_dotgraph_vec(std::shared_ptr<Node<Counter>> root, double total_time, double threshold,
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
      make_dotgraph_vec(child_node, total_time, threshold, graph_entries);
  }
}

void merge_vec(std::vector<GraphEntry>& graph_entries){
  std::vector<std::pair<std::string, int>> names;
  int size = graph_entries.size();
  for(int i = 0; i<size; i++) {
    bool entry_is_node = graph_entries[i].entry_type == node;
    bool name_in_names = false;
    for (int j = 0; j != names.size(); j++){
      if (graph_entries[i].name == names[j].first && entry_is_node){
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
    if (!name_in_names && entry_is_node){ names.emplace_back(std::make_pair(graph_entries[i].name, i)); }
  }
}

std::string get_graph_markup(std::vector<GraphEntry>& graph_entries, double total_time, int hot[3], int cool[3]){
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

GraphEntry::GraphEntry(EntryType entry_type, std::string name, double runtime, int calls,
            double total_time, int operations, std::string name_to) 
            : entry_type(entry_type), name(name), runtime(runtime), calls(calls), name_to(name_to),
            operations(operations)
{ }

// TODO: either incorporate this or remove it
//std::pair<std::string, std::string> GraphEntry::get_colours(int hot[3], int cool[3], double total_time){
//  std::string colour = blend_colours(this->runtime/total_time, hot, cool);
//  std::string fontcolour;
//  if (this->entry_type == node){
//      fontcolour = "#ffffff";
//  } else {
//      fontcolour = colour;
//  }
//  return std::make_pair(colour, fontcolour);
//}

std::string make_dotgraph(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                            double threshold){
  std::vector<GraphEntry> graph_entries;
  make_dotgraph_vec(root, total_time, threshold, graph_entries);      
  merge_vec(graph_entries);          
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