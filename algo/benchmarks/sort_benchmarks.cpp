#include <benchmark/benchmark.h>

#include <algo/sort.hpp>

#include "rand_range.hpp"

namespace
{
template <auto const& cpo, template <class...> class R, class E>
void bench_sort_cpo(benchmark::State& state)
{
    R<E> range(size_t(state.range(0)));
    for (auto _ : state) {
        state.PauseTiming();
        bench::rand_range(range); // NOLINT
        state.ResumeTiming();
        cpo(range); // NOLINT
    }
}

} // namespace

// BENCHMARK(bench_sort_cpo<algo::block_sort, std::vector, int>)
//     ->RangeMultiplier(2)
//     ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::bubble_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::heap_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::insertion_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::intro_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::merge_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::quick_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);

BENCHMARK(bench_sort_cpo<algo::tim_sort, std::vector, int>)
    ->RangeMultiplier(2)
    ->Range(8UL, 8UL << 11UL);
