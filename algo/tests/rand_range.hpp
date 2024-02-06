#pragma once

#include <random>
#include <range/v3/to_container.hpp>
#include <range/v3/view/generate.hpp>
#include <range/v3/view/take.hpp>

extern unsigned long long seed;       // NOLINT
extern unsigned long long range_size; // NOLINT

namespace testing
{

template <template <class...> class Cont = std::vector,
          class T = int,
          class Dist = std::uniform_int_distribution<T>>
inline auto rand_range(T min = 0, T max = 255)
{
    std::mt19937 gen{seed};
    Dist dist{min, max};
    return ranges::views::generate([&]() { return dist(gen); }) |
           ranges::views::take(range_size) | ranges::to<Cont<T>>();
}
} // namespace testing
