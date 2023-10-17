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

#include <ranges>
namespace algo
{

/**
 * @brief Binary search algorithm.
 *
 * Preconditions
 *  - A sorted range.
 *
 * @param rng a forward range
 * @param key the element to search for
 * @return an iterator to the element, or nullopt
 */
template <std::ranges::forward_range RNG, class CMP = std::less<>>
auto binary_search(RNG const& rng,
                   std::ranges::range_value_t<RNG> const& key, CMP cmp = CMP{})
    -> std::ranges::iterator_t<decltype(rng)>
{
    using iter_t = std::ranges::iterator_t<decltype(rng)>;

    auto size = std::ranges::range_difference_t<RNG>(std::size(rng));
    iter_t litr = std::begin(rng);
    iter_t last = litr;

    if constexpr (std::ranges::contiguous_range<RNG>) {
        last += (size - 1);
    }
    else {
        while (size > 1) {
            --size;
            ++last;
        }
    }
    iter_t ritr{last};

    while (litr != ritr) {
        iter_t mitr;
        auto dist = std::distance(litr, ritr);
        auto mid = (dist / 2);
        if constexpr (std::ranges::contiguous_range<RNG>) {
            mitr = litr + mid;
        }
        else {
            mitr = litr;
            while (mid) {
                ++mitr;
                --mid;
            }
        }
        if (cmp(*mitr, key)) {
            litr = mitr;
            ++litr;
        }
        else {
            ritr = mitr;
        }
    }
    if (litr == last && cmp(*litr, key)) {
        ++litr;
    }

    return litr;
}

} // namespace algo
