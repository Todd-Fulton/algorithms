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

#include <algorithm>
#include <ranges>
#include <array>

namespace algo
{

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void shell_sort(RNG& rng, CMP const& cmp = {})
{
    static constexpr std::array<size_t, 8> gaps{
        701, 301, 132, 57, 23, 10, 4, 1};
    std::ranges::for_each(gaps, [&](auto&& gap) {
        for (auto i = gap; i < std::size(rng); ++i) {
            auto temp = std::move(rng[i]);

            auto j = i;
            for (; (j >= gap) && (cmp(temp, rng[j - gap])); j -= gap) {
                using std::swap;
                rng[j] = std::move(rng[j - gap]);
            }
            rng[j] = std::move(temp);
        }
    });
}
} // namespace algo
