#ifndef PROFILER_SRC_MOLPRO_PROFILER_DOTGRAPH
#define PROFILER_SRC_MOLPRO_PROFILER_DOTGRAPH
#include <sstream>
#include <string>
#include <iomanip>

#include <molpro/profiler/Counter.h>
#include <molpro/profiler/Node.h>

namespace molpro {
namespace profiler {
namespace dotgraph {

std::string blend_colours(double ratio, int hot_colour[3], int cool_colour[3]);
std::string make_dotgraph(std::shared_ptr<Node<Counter>> root, double total_time, int hot[3], int cool[3],
                            double threshold);

} // namespace dotgraph
} // namespace profiler
} // namespace molpro
#endif // PROFILER_SRC_MOLPRO_PROFILER_DOTGRAPH