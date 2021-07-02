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

std::string make_dotgraph_contents(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                                    double threshold){
  std::stringstream ss;
  // get values
  auto opcount = root->counter.get_operation_count();
  auto call_count = root->counter.get_call_count();
  double time = root->counter.get_wall().cumulative_time();
  // only write box if threshold is exceeded
  if (time/total_time > threshold){
    ss << make_box(root->name, time, total_time, call_count, opcount, hot, cool);
  }
  for (auto& child : root->children){
    auto child_node = child.second;
    double child_time = child_node->counter.get_wall().cumulative_time();
    // make arrows for all children of this box
    if (child_time/total_time > threshold){
        ss << make_arrow(root->name, child_node->name, child_time, total_time, call_count, hot, cool);
        // recurse into children
        ss << make_dotgraph_contents(child_node, total_time, hot, cool, threshold);
    }
  }
  return ss.str();
}

std::string make_dotgraph(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                            double threshold){
  std::stringstream ss;
  // write header info
  ss << "digraph {\n\n";
  ss << "graph [fontname=Arial, nodesep=0.125, ranksep=0.25, fontsize=10.0];\n";
  ss << "node [fontcolor=white, fontname=Arial, height=0, shape=box, style=filled, width=0];\n";
  ss << "edge [fontname=Arial, labeldistance=4.00, penwidth=4.00];\n";
  ss << "\n";
  ss << make_dotgraph_contents(root, total_time, hot, cool, threshold);
  ss << "\n}";
  return ss.str();
}

} // namespace dotgraph
} // namespace profiler
} // namespace molpro