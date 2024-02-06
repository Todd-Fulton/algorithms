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

#include "insertion_sort.hpp"
#include <algorithm>
#include <range/v3/all.hpp>
#include <range/v3/iterator/traits.hpp>

namespace algo
{

namespace _tim_sort
{
// Merge function merges the sorted runs
template <class CMP = std::less<>>
void merge(std::ranges::viewable_range auto& rng,
           size_t left_size,
           size_t right_size,
           CMP const& cmp = {})
{
    using T =
        std::ranges::range_value_t<std::remove_cvref_t<decltype(rng)>>;

    using diff_t = std::ranges::range_difference_t<
        std::remove_cvref_t<decltype(rng)>>;

    auto begin = ranges::begin(rng);
    auto end = ranges::end(rng);
    // Original array is broken in two
    // parts left and right array
    // TODO: Move vector allocation out of merge function.
    std::vector<T> left(left_size);
    std::vector<T> right(right_size);

    ranges::copy(begin, begin + diff_t(left_size), std::begin(left));
    ranges::copy(begin + diff_t(left_size), end, std::begin(right));

    size_t i = 0;
    size_t j = 0;

    // After comparing, we
    // merge those two array
    // in larger sub array
    while (i < left_size && j < right_size) {
        if (cmp(left[i], right[j])) {
            *begin = std::move(left[i]);
            ++i;
        }
        else {
            *begin = std::move(right[j]);
            ++j;
        }
        ++begin;
    }

    // Copy remaining elements of
    // left, if any
    while (i < left_size) {
        *begin = std::move(left[i]);
        ++begin;
        ++i;
    }

    // Copy remaining element of
    // right, if any
    while (j < right_size) {
        *begin = std::move(right[j]);
        ++begin;
        ++j;
    }
}

} // namespace _tim_sort
template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void tim_sort(RNG& rng, CMP const& cmp = {})
{
    using diff_t = std::ranges::range_difference_t<
        std::remove_cvref_t<decltype(rng)>>;

    static constexpr size_t RUN = 32;

    // Sort individual subranges of size RUN
    ranges::for_each(ranges::chunk_view(rng, diff_t(RUN)),
                     [&](auto&& chunk) { insertion_sort(chunk, cmp); });

    // Start merging from size RUN * 2^0, RUN * 2^1, RUN * 2^2, ...
    for (size_t size = RUN; size < std::size(rng); size *= 2) {
        ranges::for_each(
            ranges::chunk_view(rng, diff_t(size * 2)), [&](auto&& chunk) {
            if (std::ranges::size(chunk) > size) {
                size_t left = size;
                size_t right = std::ranges::size(chunk) - left;
                    _tim_sort::merge(chunk, left, right, cmp);
                }
            });
    }
}
} // namespace algo
