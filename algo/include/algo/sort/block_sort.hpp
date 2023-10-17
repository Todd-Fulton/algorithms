/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 **/

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
