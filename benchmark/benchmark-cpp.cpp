#include <memory>
#include <chrono>
#include <molpro/Profiler.h>
#ifdef MOLPRO_PROFILER_MPI
#include <mpi.h>
#endif
int main(int argc, char* argv[]) {
#ifdef MOLPRO_PROFILER_MPI
  MPI_Init(&argc, &argv);
#endif
  molpro::Profiler p{"test"};
  auto start = std::chrono::system_clock::now();
  size_t repeat = 1000000;
  if (argc > 1)
    repeat = std::stoi(argv[1]);
  std::cout << repeat << " instances of push()" << std::endl;
  for (auto i = 0; i < repeat; i++) {
    p.push("test");
  }
  std::cout << p << std::endl;
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()/repeat
            << std::endl;

#ifdef MOLPRO_PROFILER_MPI
  MPI_Finalize();
#endif
}