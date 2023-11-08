/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as
 *published by the Free Software Foundation, either version 3 of the
 *License, or (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *License along with this program.  If not, see
 *<https://www.gnu.org/licenses/>.
 **/

#pragma once

#include <range/v3/all.hpp>
#include <set>

namespace algo
{

template <ranges::contiguous_range RNG, class CMP = std::less<>>
void tree_sort(RNG& rng, CMP const& cmp = {})
{
    using T = ranges::range_value_t<RNG>;

    std::set<T, CMP> set{cmp};
    ranges::for_each(rng,
                     [&](auto& item) { set.insert(std::move(item)); });
    auto itr = std::begin(rng);
    ranges::for_each(set, [&](auto& item) {
        *itr = std::move(item);
        ++itr;
    });
}
} // namespace algo
