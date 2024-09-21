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

#include "ordering.hpp"
#include "sorted.hpp"
#include <algo/partition.hpp>
#include <algo/sort/heap_sort.hpp>
#include <algo/sort/insertion_sort.hpp>

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/view/common.hpp>
#include <range/v3/view/subrange.hpp>

#include <cmath>
#include <iterator>

#include <algo/prelude.hpp>

namespace algo
{

/// Implementation details of the intro_sort algorithm.
namespace _intro_sort
{

template <class Relation, class Projection, class Partition, class Sort>
struct Algorithm
{
    [[no_unique_address]] Relation relation{};
    [[no_unique_address]] Projection projection{};
    [[no_unique_address]] Partition partition{};
    [[no_unique_address]] Sort sort{};

    /// Recursive function to sort a range using the introsort method.
    template <std::random_access_iterator I>
    constexpr void recursion(this auto&& self, I first, I last, int depthLimit)
    // TODO: noexcept
    {
        // If the size of the range is small enough, use insertion sort.
        if (std::distance(first, last) *
                ranges::iter_difference_t<I>(sizeof(std::iter_value_t<I>)) <
            128) {
            insertion_sort(first, last, self.relation, self.projection);
        }
        // If the depth limit has been reached, use heap sort instead.
        else if (depthLimit == 0) {
            self.sort(first, last, self.relation, self.projection);
        }
        // Otherwise, partition around a pivot and recurse on both sides of the pivot.
        else {
            auto elem = first + (std::distance(first, last) / 2);
            auto pivot = self.partition(
                first,
                last,
                [&, x = *elem](auto&& element) { return self.relation(element, x); },
                self.projection);
            self.recursion(first, pivot, depthLimit - 1);
            self.recursion(pivot, last, depthLimit - 1);
        }
    }

    template <class Range>
    requires ranges::random_access_range<Range> and ranges::sized_range<Range>
    constexpr auto operator()(this auto&& self, Range&& range)
        -> sorted<Range, Relation, Projection>
    // TODO: noexcept
    {
        // Get the size of the range and calculate the depth limit for recursion.
        auto size = ranges::distance(range);
        int depth = static_cast<int>(2 * std::log(size));

        // Use recursion to sort the range.
        if constexpr (!ranges::common_range<Range>) {
            auto cv = ranges::views::common(range);
            self.recursion(ranges::begin(cv), ranges::end(cv));
        }
        else {
            self.recursion(ranges::begin(range), ranges::end(range), depth);
        }
        return {std::forward<Range>(range),
                std::forward_like<decltype(self)>(self.relation),
                std::forward_like<decltype(self)>(self.projection)};
    }

    template <class Itr, class Sentinel>
    requires ranges::random_access_iterator<Itr> and ranges::sentinel_for<Sentinel, Itr>
    constexpr auto operator()(this auto&& self, Itr&& first, Sentinel&& last)
        -> sorted<ranges::subrange<Itr, Sentinel>, Relation, Projection>
    {
        return std::forward<decltype(self)>(
            ranges::subrange(std::forward<Itr>(first), std::forward<Sentinel>(last)));
    }
};

template <class Relation, class Projection, class PartitionTag, class SortTag>
struct _adapter
{
    struct type;
};

template <class Relation, class Projection, class PartitionTag, class SortTag>
using adapter = _adapter<std::decay_t<Relation>,
                         std::decay_t<Projection>,
                         std::decay_t<PartitionTag>,
                         std::decay_t<SortTag>>::type;

} // namespace _intro_sort

inline constexpr struct intro_sort_fn
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity,
              class PartitionTag = unifex::tag_t<lomuto_partition>,
              class SortTag = unifex::tag_t<heap_sort>>
    requires(not(ranges::range<Relation> or ranges::range<Projection> or
                 ranges::range<PartitionTag> or ranges::range<SortTag>))
    static constexpr auto operator()(Relation&& relation,
                                     Projection&& projection,
                                     PartitionTag&& partition,
                                     SortTag&& sort)
        noexcept(std::is_nothrow_constructible_v<
                 _intro_sort::adapter<Relation, Projection, PartitionTag, SortTag>,
                 Relation,
                 Projection,
                 PartitionTag,
                 SortTag>)
            -> _intro_sort::adapter<Relation, Projection, PartitionTag, SortTag>
    {
        return {std::forward<Relation>(relation),
                std::forward<Projection>(projection),
                std::forward<PartitionTag>(partition),
                std::forward<SortTag>(sort)};
    }

    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity,
              class PartitionTag = unifex::tag_t<branchless_lomuto_partition>,
              class SortTag = unifex::tag_t<heap_sort>>
    requires unifex::
        tag_invocable<intro_sort_fn, Range, Relation, Projection, PartitionTag, SortTag>
        static constexpr auto operator()(Range&& rng,
                                         Relation&& relation = {},
                                         Projection&& projection = {},
                                         PartitionTag&& partition = {},
                                         SortTag&& subsort = {})
            noexcept(unifex::is_nothrow_tag_invocable_v<intro_sort_fn,
                                                        Range,
                                                        Relation,
                                                        Projection,
                                                        PartitionTag,
                                                        SortTag>)
                -> unifex::tag_invoke_result_t<intro_sort_fn,
                                               Range,
                                               Relation,
                                               Projection,
                                               PartitionTag,
                                               SortTag>
    {
        return tag_invoke(intro_sort_fn{},
                          std::forward<Range>(rng),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection),
                          std::forward<PartitionTag>(partition),
                          std::forward<SortTag>(subsort));
    }

    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity,
              class PartitionTag = unifex::tag_t<branchless_lomuto_partition>,
              class SortTag = unifex::tag_t<heap_sort>>
    requires unifex::tag_invocable<intro_sort_fn,
                                   Itr,
                                   Sentinel,
                                   Relation,
                                   Projection,
                                   PartitionTag,
                                   SortTag>
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {},
                                     PartitionTag&& partition = {},
                                     SortTag&& subsort = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<intro_sort_fn,
                                                    Itr,
                                                    Sentinel,
                                                    Relation,
                                                    Projection,
                                                    PartitionTag,
                                                    SortTag>)
            -> unifex::tag_invoke_result_t<intro_sort_fn,
                                           Itr,
                                           Sentinel,
                                           Relation,
                                           Projection,
                                           PartitionTag,
                                           SortTag>
    {
        return tag_invoke(intro_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection),
                          std::forward<PartitionTag>(partition),
                          std::forward<SortTag>(subsort));
    }

    template <class Range,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity,
              class PartitionTag = hoare_partition_fn,
              class SortFn = heap_sort_fn>
    // TODO: reduce restriction on random_access_range if able
    requires(not unifex::tag_invocable<intro_sort_fn,
                                       Range,
                                       Relation,
                                       Projection,
                                       PartitionTag,
                                       SortFn> and
             ranges::random_access_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {},
                                     PartitionTag&& partition = {},
                                     SortFn&& subsort = {})
        noexcept(noexcept(_intro_sort::Algorithm{
            std::forward<Relation>(relation),
            std::forward<Projection>(projection),
            std::forward<PartitionTag>(partition),
            std::forward<SortFn>(subsort)}(std::forward<Range>(range))))
            -> decltype(_intro_sort::Algorithm{
                std::forward<Relation>(relation),
                std::forward<Projection>(projection),
                std::forward<PartitionTag>(partition),
                std::forward<SortFn>(subsort)}(std::forward<Range>(range)))
    {
        return _intro_sort::Algorithm{std::forward<Relation>(relation), // NOLINT
                                      std::forward<Projection>(projection),
                                      std::forward<PartitionTag>(partition),
                                      std::forward<SortFn>(subsort)}(
            std::forward<Range>(range));
    }

    template <class Itr,
              class Sentinel,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity,
              class PartitionTag = hoare_partition_fn,
              class SortFn = heap_sort_fn>
    // TODO: reduce restriction on random_access_range if able
    requires(
        not unifex::tag_invocable<intro_sort_fn,
                                  Itr,
                                  Sentinel,
                                  Relation,
                                  Projection,
                                  PartitionTag,
                                  SortFn> and
        ranges::random_access_iterator<std::remove_cvref_t<Itr>> and
        ranges::sentinel_for<std::remove_cvref_t<Sentinel>, std::remove_cvref_t<Itr>> and
        ranges::indirect_strict_weak_order<Relation,
                                           ranges::projected<Itr, Projection>,
                                           ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {},
                                     PartitionTag&& partition = {},
                                     SortFn&& subsort = {})
        noexcept(noexcept(_intro_sort::Algorithm{std::forward<Relation>(relation),
                                                 std::forward<Projection>(projection),
                                                 std::forward<PartitionTag>(partition),
                                                 std::forward<SortFn>(subsort)}(
            std::forward<Itr>(first), std::forward<Sentinel>(last))))
            -> decltype(_intro_sort::Algorithm{
                std::forward<Relation>(relation),
                std::forward<Projection>(projection),
                std::forward<PartitionTag>(partition),
                std::forward<SortFn>(subsort)}(std::forward<Itr>(first),
                                               std::forward<Sentinel>(last)))
    {
        return _intro_sort::Algorithm{std::forward<Relation>(relation),
                                      std::forward<Projection>(projection),
                                      std::forward<PartitionTag>(partition),
                                      std::forward<SortFn>(subsort)}(
            std::forward<Itr>(first), std::forward<Sentinel>(last));
    }
} intro_sort;

namespace _intro_sort
{
template <class Relation, class Projection, class PartitionTag, class SortTag>
struct _adapter<Relation, Projection, PartitionTag, SortTag>::type final
{
    [[no_unique_address]] Relation relation;
    [[no_unique_address]] Projection projection;
    [[no_unique_address]] PartitionTag partition;
    [[no_unique_address]] SortTag subsort;

    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(Range&& range, Adapter&& adapter)
    {
        return intro_sort(std::forward<Range>(range),
                          std::forward_like<Adapter>(adapter.relation),
                          std::forward_like<Adapter>(adapter.projection),
                          std::forward_like<Adapter>(adapter.partition),
                          std::forward_like<Adapter>(adapter.subsort));
    }
};

} // namespace _intro_sort
} // namespace algo

#include <algo/prologue.hpp>
