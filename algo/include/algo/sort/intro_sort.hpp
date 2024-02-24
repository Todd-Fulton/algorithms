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

#include <algo/partition.hpp>
#include <algo/sort/heap_sort.hpp>
#include <algo/sort/insertion_sort.hpp>
#include <algo/sort/ordering.hpp>

#include <cmath>
#include <iterator>
#include <range/v3/iterator/concepts.hpp>
#include <range/v3/view/subrange.hpp>

#include <algo/prelude.hpp>

namespace algo
{

/// Implementation details of the intro_sort algorithm.
namespace _intro_sort
{
/// Recursive function to sort a range using the introsort method.
template <std::random_access_iterator I>
void recursion(I first, I last, int depthLimit, auto const& compare)
{
    // If the size of the range is small enough, use insertion sort.
    if (std::distance(first, last) *
            ranges::iter_difference_t<I>(sizeof(std::iter_value_t<I>)) <
        128) {
        insertion_sort(first, last, compare);
    }
    // If the depth limit has been reached, use heap sort instead.
    else if (depthLimit == 0) {
        heap_sort(first, last, compare);
    }
    // Otherwise, partition around a pivot and recurse on both sides of the pivot.
    else {
        auto elem = first + (std::distance(first, last) / 2);
        auto pivot = branchless_lomuto_partition(
            first, last, [&, x = *elem](auto&& element) { return compare(element, x); });
        recursion(first, pivot, depthLimit - 1, compare);
        recursion(pivot, last, depthLimit - 1, compare);
    }
}

template <std::random_access_iterator I, ranges::sentinel_for<I> S>
void algorithm(I first, S end, auto const& compare)
{
    // Get the size of the range and calculate the depth limit for recursion.
    auto size = ranges::distance(first, end);
    int depth = static_cast<int>(2 * std::log(size));

    // Use recursion to sort the range.
    auto last = first;
    std::advance(last, size);
    recursion(first, last, depth, compare);
}

template <class Ordering>
struct _adapter
{
    struct type;
};

template <class Ordering>
using adapter = _adapter<Ordering>::type;

namespace _cpo
{
inline constexpr struct _fn
{
    template <class Range, class OP>
    requires(ranges::sized_range<Range> and
             ranges::indirect_binary_predicate_<predicate_for_t<OP>,
                                                ranges::iterator_t<Range>,
                                                ranges::iterator_t<Range>>)
    static constexpr auto operator()(Range&& rng, OP&& compare)
    {
        _intro_sort::algorithm(begin(rng), end(rng), std::forward<OP>(compare));
        return std::forward<Range>(rng);
    }

    template <class Itr, class Sentinel, class OP>
    requires(ranges::random_access_iterator<Itr> and
             ranges::sentinel_for<Sentinel, Itr> and
             ranges::indirect_binary_predicate_<predicate_for_t<OP>, Itr, Itr>)
    static constexpr auto operator()(Itr&& start, Sentinel&& end, OP&& compare)
    {
        return tag_invoke(_fn{},
                          std::forward<Itr>(start),
                          std::forward<Sentinel>(end),
                          std::forward<OP>(compare));
    }

    static constexpr auto operator()(auto&& compare) noexcept
        -> adapter<decltype(compare)>
    requires(not ranges::range<decltype(compare)>)
    {
        return {FWD(compare)};
    }

private:
    template <class Itr, class Sentinel, class OP>
    requires(ranges::random_access_iterator<Itr> and ranges::sentinel_for<Sentinel, Itr>)
    friend constexpr auto tag_invoke([[maybe_unused]] _fn /*unused*/,
                                     Itr&& first,
                                     Sentinel&& last,
                                     [[maybe_unused]] OP&& ordering_or_predicate)
        noexcept(
            noexcept(_intro_sort::algorithm(std::forward<Itr>(first),
                                            std::forward<Sentinel>(last),
                                            std::forward<OP>(ordering_or_predicate))))
            -> decltype(_intro_sort::algorithm(std::forward<Itr>(first),
                                               std::forward<Sentinel>(last),
                                               std::forward<OP>(ordering_or_predicate)))
    {
        return _intro_sort::algorithm(std::forward<Itr>(first),
                                      std::forward<Sentinel>(last),
                                      std::forward<OP>(ordering_or_predicate));
    }

} intro_sort;
} // namespace _cpo

} // namespace _intro_sort

using _intro_sort::_cpo::intro_sort;

namespace _intro_sort
{
template <class OP>
struct _adapter<OP>::type final
{
    [[no_unique_address]] OP op;

    template <class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(ranges::random_access_range auto&& range,
                                    Adapter&& adapter)
    {
        return intro_sort(FWD(range), std::forward_like<Adapter>(adapter.op));
    }
};

} // namespace _intro_sort
} // namespace algo

#include <algo/prologue.hpp>
