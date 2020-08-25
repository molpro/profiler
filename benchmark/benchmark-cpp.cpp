#include <memory>
#include <chrono>
#include <molpro/Profiler.h>
#ifdef MOLPRO_PROFILER_MPI
#include <mpi.h>
#include <sys/time.h>
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
  std::cout << repeat << " instances of Profiler::push()" << std::endl;
  for (auto i = 0; i < repeat; i++) {
    p.push("test");
  }
  std::cout << p << std::endl;
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: "
            << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / repeat
            << std::endl;

  std::cout << repeat << " instances of 2*gettimeofday()" << std::endl;
  start = std::chrono::system_clock::now();
  auto wall = (double)0;
  for (auto i = 0; i < repeat*2; i++) {
    struct timeval time;
    if (!gettimeofday(&time, NULL))
      wall = (double)time.tv_sec + (double)time.tv_usec * .000001;
  }
  std::cout << p << std::endl;
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: "
            << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / repeat
            << std::endl;


  std::cout << repeat << " instances of 2*chrono::system_clock::now()" << std::endl;
  start = std::chrono::system_clock::now();
  auto result = std::chrono::system_clock::now();
  for (auto i = 0; i < repeat; i++) {
    result += std::chrono::system_clock::now() - std::chrono::system_clock::now();
  }
  std::cout << p << std::endl;
  std::cout << "Elapsed time: " << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count()
            << std::endl;
  std::cout << "Elapsed time per call: "
            << (std::chrono::duration<double>(std::chrono::system_clock::now() - start)).count() / repeat
            << std::endl;

#ifdef MOLPRO_PROFILER_MPI
  MPI_Finalize();
#endif
}