#pragma once

#include <algorithm>
#include <bit>
#include <ranges>

#include "insertion_sort.hpp"

namespace algo
{

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void block_sort(RNG& rng, CMP cmp = CMP{})
{
    auto size = std::size(rng);
    auto power_of_two = std::bit_floor(size);
    double scale = size / double(power_of_two);

    // insertion sort 16-31 items at a time
    auto slide_vw = std::views::slide(rng, size_t(16 * scale));
    std::ranges::for_each(slide_vw,
                          [&](std::ranges::viewable_range auto&& x) {
                              insertion_sort(x, cmp);
                          });
    // TODO:
}
}
