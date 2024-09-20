#pragma once

#include <random>
#include <range/v3/to_container.hpp>
#include <range/v3/view/generate.hpp>
#include <range/v3/view/take.hpp>


extern unsigned long long seed;       // NOLINT
extern unsigned long long range_size; // NOLINT

namespace testing
{

template <class T = int, class Dist = std::uniform_int_distribution<T>>
inline auto rand_int(T min = 0, T max = 255) -> T {
    static std::mt19937 gen{seed};
    Dist dist{min, max};
    return dist(gen);
}

template <template <class...> class Cont = std::vector,
          class T = int,
          class Dist = std::uniform_int_distribution<T>>
inline auto rand_range(T min = 0, T max = 255) -> Cont<T>
{
    return ranges::views::generate([&]() { return rand_int<T, Dist>(min, max); }) |
           ranges::views::take(range_size) | ranges::to<Cont<T>>();
}

} // namespace testing
