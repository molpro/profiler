#include "ProfilerMPI.h"
#include <cmath>

namespace molpro {
namespace profiler {

ProfilerMPI::ProfilerMPI(const std::string& name,
                         sortMethod sortBy,
                         int level,
                         MPI_Comm communicator,
                         bool cpu)
    : ProfilerSerial(name, sortBy, level, false, cpu), m_communicator(communicator) {
  reset(name);
  active(level);
}

ProfilerMPI::resultMap ProfilerMPI::totals() const {
  ProfilerMPI thiscopy = *this; // take a copy so that we can run stopall yet be const, and so that we can sum globally
  thiscopy.stopall();
  while (thiscopy.results.erase(""))
    ;
  int rank;
  MPI_Comm_rank(m_communicator, &rank);
  std::string key;
  // use the table on the master node as definitive, since others may be missing entries
  int n = thiscopy.results.size();
  MPI_Bcast(&n, 1, MPI_INT, 0, m_communicator);
  for (int i = 0; i < n; i++) {
    int l;
    if (rank == 0) {
      resultMap::iterator s = thiscopy.results.begin();
      for (int j = 0; j < i; j++)
        s++;
      key = s->first;
      l = key.size();
    }
    MPI_Bcast(&l, 1, MPI_INT, 0, m_communicator);
    key.resize(l);
    MPI_Bcast(&key[0], l, MPI_CHAR, 0, m_communicator);
    struct ProfilerMPI::resources ss = thiscopy.results[key];
    ss.parent = this;
    int len = 1;
    double val = ss.wall;
    MPI_Allreduce(&val, &(ss.wall), len, MPI_DOUBLE, MPI_MAX, m_communicator);
    val = ss.cpu;
    MPI_Allreduce(&val, &(ss.cpu), len, MPI_DOUBLE, MPI_SUM, m_communicator);
    int calls = ss.calls;
    MPI_Allreduce(&calls, &(ss.calls), len, MPI_INT, MPI_MAX, m_communicator);
    long operations = ss.operations;
    MPI_Allreduce(&operations, &(ss.operations), len, MPI_LONG, MPI_SUM, m_communicator);
    int64_t stack = ss.stack;
    MPI_Allreduce(&stack, &(ss.stack), len, MPI_LONG_LONG_INT, MPI_MAX, m_communicator);
    thiscopy.results[key] = ss;
  }
  for (auto &x : thiscopy.results)
    x.second.parent = this;
  thiscopy.accumulate(thiscopy.results);
  return thiscopy.results;
}

std::ostream &operator<<(std::ostream &os, ProfilerMPI &obj) {
  int rank;
  MPI_Comm_rank(obj.m_communicator, &rank);
  auto str = obj.str();
  if (rank == 0)
    return os << str << std::endl;
  else
    return os;
}
} // namespace profiler
} //  namespace molpro
