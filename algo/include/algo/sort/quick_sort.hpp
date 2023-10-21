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

#include <ranges>
namespace algo
{

namespace _quick_sort
{
template <class BItr, class EItr, class CMP = std::less<>>
auto partition(BItr&& begin, EItr&& end, CMP const& cmp)
requires(std::random_access_iterator<std::remove_cvref_t<BItr>> and
         std::random_access_iterator<std::remove_cvref_t<EItr>>)
{
    auto const pivot = *(begin + (std::distance(begin, end) / 2));
    auto i = begin;
    auto j = end - 1;

    while (true) {
        for(; cmp(*i, pivot); ++i) {}
        for(; cmp(pivot, *j); --j) {}

        if (i >= j) {
            return j;
        }
        using std::swap;
        swap(*i, *j);
    }
}
} // namespace _quick_sort

template <class BItr, class EItr, class CMP = std::less<>>
void quick_sort(BItr&& begin, EItr&& end, CMP const& cmp)
requires(std::random_access_iterator<std::remove_cvref_t<BItr>> and
         std::random_access_iterator<std::remove_cvref_t<EItr>>)
{
    if (begin < end) {
        auto part_itr = _quick_sort::partition(begin, end, cmp);
        quick_sort(begin, part_itr, cmp);
        quick_sort(++part_itr, end, cmp);
    }
}

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void quick_sort(RNG& rng, CMP const& cmp = {})
{
    quick_sort(std::begin(rng), std::end(rng), cmp);
}
} // namespace algo
