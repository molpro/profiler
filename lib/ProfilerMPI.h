#ifndef PROFILER_MPI_H
#define PROFILER_MPI_H
#include "ProfilerSerial.h"
#include "mpi.h"

/*!
 * \brief MPI version of the profiler
 *
 * All process in the communicator must create Profiler instance collectively.
 */
class ProfilerMPI : public ProfilerSerial {
public:
  /*!
   * \brief Profiler construct a named instance.
   * \param name the title of this object.
   * \param sortBy Criterion for sorting printed result table.
   * \param level
   * A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
   * \param communicator The MPI communicator over which statistics should be aggregated.
   */
  ProfilerMPI(const std::string &name, sortMethod sortBy = wall, int level = INT_MAX,
           MPI_Comm communicator = MPI_COMM_WORLD);

  /*!
   * \brief Obtain a summary of the resources used for each category.
   * Must be called by all MPI processes collectively.
   * \return std::map of \ref resources
   */
  virtual resultMap totals() const override;

private:
  const MPI_Comm m_communicator;
};


#endif // PROFILER_MPI_H
