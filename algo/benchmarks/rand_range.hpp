#pragma once

#include <range/v3/algorithm/for_each.hpp>

#include <random>

namespace bench
{

template <template <class...> class Cont = std::vector,
          class T = int,
          class Dist = std::uniform_int_distribution<T>>
inline auto rand_range(Cont<T>& range, T min = 0, T max = 255)
{
    std::random_device rand_dev;
    std::mt19937 gen{rand_dev()};
    Dist dist{min, max};
    ranges::for_each(range, [&](auto& elem) { elem = dist(gen); });
}

template <class T = int, class Dist = std::uniform_int_distribution<T>>
inline auto rand_key(T min = 0, T max = 255)
{
    std::random_device rand_dev;
    std::mt19937 gen{rand_dev()};
    Dist dist(min, max);
    return dist(gen);
}
} // namespace bench
