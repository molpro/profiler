#ifndef MOLPRO_PROFILER_PROFILER_MPI_H
#define MOLPRO_PROFILER_PROFILER_MPI_H
#include "molpro/Profiler/ProfilerSerial.h"
#include <mpi.h>
#undef PROFILER_DEFAULT_KEY
#define PROFILER_DEFAULT_KEY MPI_COMM_WORLD

namespace molpro {
namespace profiler {

/*!
 * \brief MPI version of the profiler
 *
 * All process in the communicator must create Profiler instance collectively.
 */
class ProfilerMPI : public ProfilerSerial {
public:
  using key_t = MPI_Comm; //!< extra key to provide consistent interface with ProfilerMPI. No purpose
  /*!
   * \brief Profiler construct a named instance.
   * @warning If using anything other than the default MPI communicator MPI_COMM_WORLD, for example from use of GA or
   * PPIDD, then you must pass the communicator explicitly. \param name the title of this object. \param sortBy
   * Criterion for sorting printed result table. \param level A large value means that data will always be accumulated;
   * zero means that calls to start and stop do nothing. \param communicator The MPI communicator over which statistics
   * should be aggregated. \param cpu Whether to poll CPU time
   */
  ProfilerMPI(const std::string& name, sortMethod sortBy = wall, int level = INT_MAX,
              MPI_Comm communicator = PROFILER_DEFAULT_KEY, bool cpu = false);

  /*!
   * \brief Obtain a summary of the resources used for each category.
   * Must be called by all MPI processes collectively.
   * \return std::map of ProfilerSerial::resources
   */
  virtual resultMap totals() const override;

  //! Writes summary of profile to the stream, with end of line
  friend std::ostream& operator<<(std::ostream& os, ProfilerMPI& obj);

protected:
  const MPI_Comm m_communicator;
};

} // namespace profiler
} // namespace molpro

#endif // MOLPRO_PROFILER_PROFILER_MPI_H
