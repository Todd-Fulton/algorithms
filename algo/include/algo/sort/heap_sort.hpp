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

#include <iterator>
#include <range/v3/iterator/concepts.hpp>
#include <range/v3/iterator/operations.hpp>
#include <range/v3/range/traits.hpp>
namespace algo
{

namespace _heap_sort
{

constexpr auto i_parent(auto x)
{
    if (x == 0) {
        // TODO: use proper exception.
        throw std::exception();
    }
    return (x - 1) / 2;
}

constexpr auto i_left_child(auto x) noexcept
{
    return 2 * x + 1;
}

constexpr auto i_right_child(auto x) noexcept
{
    return 2 * x + 2;
}

inline auto leaf_search(auto const& itr, auto i, auto end, auto const& cmp)
{
    auto j = i;
    auto lc = i_left_child(j);
    auto rc = lc + 1;
    while (rc < end) {
        if (!cmp(*(itr + rc), *(itr + lc))) {
            j = rc;
        }
        else {
            j = lc;
        }
        lc = i_left_child(j);
        rc = lc + 1;
    }

    lc = i_left_child(j);
    if (lc < end) {
        j = lc;
    }
    return j;
}

inline void sift_down(auto& itr, auto i, auto end, auto const& cmp)
{
    auto j = leaf_search(itr, i, end, cmp);
    while (!cmp(*(itr + i), *(itr + j)) and j != 0) {
        j = i_parent(j);
    }
    while (j > i) {
        ranges::iter_swap(itr + i, itr + j);
        j = i_parent(j);
    }
};

inline void heapify(auto& itr, auto size, auto const& cmp)
{
    auto start = i_parent(size + 1);

    while (start > 0) {
        --start;
        sift_down(itr, start, size, cmp);
    }
};
} // namespace _heap_sort

template <std::random_access_iterator I,
          ranges::sentinel_for<I> Sentinel,
          class CMP = std::less<>>
void heap_sort(I start, Sentinel end, CMP cmp = CMP{})
{
    using _heap_sort::heapify;
    using _heap_sort::sift_down;
    using std::swap;

    auto size = ranges::distance(start, end);
    heapify(start, size, cmp);

    while (size > 1) {
        --size;
        ranges::iter_swap(start + size, start);
        sift_down(start, 0, size, cmp);
    }
}

template <ranges::random_access_range Range,
          class Compare = std::less<ranges::range_value_t<Range>>>
void heap_sort(Range&& range, Compare const& compare = {})
{
    heap_sort(ranges::begin(range), ranges::end(range), compare);
}
} // namespace algo
