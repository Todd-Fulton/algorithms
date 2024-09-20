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


#include <range/v3/range_concepts.hpp>
namespace algo {

template <ranges::forward_range RNG, class CMP = std::less<>>
void selection_sort(RNG& rng, CMP const& cmp = CMP{})
{
    auto i_itr = std::begin(rng);
    auto j_itr = std::begin(rng);
    auto count = std::size(rng) - 1;

    for (auto i = 0UL; i < count; std::advance(i_itr, 1), ++i)
    {

        auto sel = i_itr;
        j_itr = i_itr;
        ++j_itr;

        for (; j_itr != ranges::end(rng); std::advance(j_itr, 1))
        {
            if(cmp(*j_itr, *sel)) {
                sel = j_itr;
            }
        }

        if (sel != i_itr)
        {
            ranges::iter_swap(i_itr, sel);
        }
    }
}
}
