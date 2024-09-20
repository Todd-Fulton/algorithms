find_package(benchmark CONFIG REQUIRED)

macro(algo_add_benchmark name)
  add_executable(${name} ${name}.cpp rand_range.hpp)
  target_link_libraries(${name} PRIVATE algorithms benchmark::benchmark
                                        benchmark::benchmark_main)
endmacro()

algo_add_benchmark(sort_benchmarks)
algo_add_benchmark(search_benchmarks)
