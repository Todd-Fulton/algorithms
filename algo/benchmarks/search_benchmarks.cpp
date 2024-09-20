#include "rand_range.hpp"
#include <algo/search.hpp>
#include <algo/sort.hpp>
#include <benchmark/benchmark.h>

#include <list>

namespace
{
template <auto const& cpo, template <class...> class R, class E>
void bench_search_cpo(benchmark::State& state)
{
    R<E> range(size_t(state.range(0)));
    bench::rand_range(range, E(0), E(state.range(0) / 2)); // NOLINT
    auto key = bench::rand_key<E>(E(0), E(state.range(0) / 2));
    auto sorted_vw = algo::insertion_sort(range);
    for (auto _ : state) {
        auto itr = cpo(sorted_vw, key); // NOLINT
        benchmark::DoNotOptimize(itr);
    }
}

} // namespace

BENCHMARK(bench_search_cpo<algo::binary_search, std::vector, int16_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::linear_search, std::vector, int16_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::binary_search, std::vector, int32_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::linear_search, std::vector, int32_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::binary_search, std::vector, int64_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::linear_search, std::vector, int64_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::linear_search, std::list, int16_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::linear_search, std::list, int32_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

BENCHMARK(bench_search_cpo<algo::linear_search, std::list, int64_t>)
    ->RangeMultiplier(2)
    ->Range(16UL, 8UL << 12UL)
    ->Threads(6);

