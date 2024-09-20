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
#include <range/v3/range_concepts.hpp>

#include <algo/prelude.hpp>
#include <type_traits>
#include <unifex/tag_invoke.hpp>

// TODO:
// [ ] Add concept constrains to functions and member functions

namespace algo
{

namespace _lomuto_partition
{

template <class Low, class Last, class Predicate, class Projection = ranges::identity>
requires(ranges::forward_iterator<Low> and ranges::sentinel_for<Last, Low> and
         ranges::indirect_unary_predicate<Predicate, ranges::projected<Low, Projection>>)
constexpr auto algorithm(Low low,
                         Last last,
                         Predicate&& predicate,
                         Projection&& projection = {})
{
    auto cursor = low;
    for (; cursor != last; ++cursor) {
        if (predicate(projection(*cursor))) {
            ranges::iter_swap(low, cursor);
            ++low;
        }
    }
    return low;
}

template <class Predicate, class Projection>
struct _adapter
{
    struct type;
};

template <class Predicate, class Projection>
using adapter = _adapter<Predicate, Projection>::type;

} // namespace _lomuto_partition

inline constexpr struct lomuto_partition_fn
{
    template <class Predicate, class Projection = std::identity>
    requires(!ranges::range<Predicate> and !ranges::range<Projection>)
    static constexpr auto operator()(Predicate&& predicate, Projection&& projection = {})
        noexcept(std::is_nothrow_constructible_v<
                 _lomuto_partition::adapter<Predicate, Projection>,
                 Predicate,
                 Projection>) -> _lomuto_partition::adapter<Predicate, Projection>
    {
        return {std::forward<Predicate>(predicate), std::forward<Projection>(projection)};
    }

    template <class Range, class Predicate, class Projection = ranges::identity>
    requires unifex::tag_invocable<lomuto_partition_fn, Range, Predicate, Projection>
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = {}) //
        noexcept(unifex::is_nothrow_tag_invocable_v<lomuto_partition_fn,
                                                    Range,
                                                    Predicate,
                                                    Projection>)
            -> unifex::
                tag_invoke_result_t<lomuto_partition_fn, Range, Predicate, Projection>
    {
        return tag_invoke(lomuto_partition_fn{},
                          std::forward<Range>(range),
                          std::forward<Predicate>(predicate),
                          std::forward<Projection>(projection));
    }

    template <class Iter,
              class Sentinel,
              class Predicate,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<lomuto_partition_fn,
                                   Iter,
                                   Sentinel,
                                   Predicate,
                                   Projection> and
                 ranges::sentinel_for<Sentinel, Iter>
    static constexpr auto operator()(Iter&& first,
                                     Sentinel&& last,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<lomuto_partition_fn,
                                                    Iter,
                                                    Sentinel,
                                                    Predicate,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<lomuto_partition_fn,
                                           Iter,
                                           Sentinel,
                                           Predicate,
                                           Projection>
    {
        return tag_invoke(lomuto_partition_fn{},
                          std::forward<Iter>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Predicate>(predicate),
                          std::forward<Projection>(projection));
    }

    template <class Range, class Predicate, class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<lomuto_partition_fn, Range, Predicate, Projection> and
             ranges::forward_range<Range> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>> and
             ranges::viewable_range<Range>)
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(
            noexcept(_lomuto_partition::algorithm(ranges::begin(range),
                                                  ranges::end(range),
                                                  std::forward<Predicate>(predicate),
                                                  std::forward<Projection>(projection))))
            -> decltype(_lomuto_partition::algorithm(
                ranges::begin(range),
                ranges::end(range),
                std::forward<Predicate>(predicate),
                std::forward<Projection>(projection)))
    {
        return _lomuto_partition::algorithm(ranges::begin(range),
                                            ranges::end(range),
                                            std::forward<Predicate>(predicate),
                                            std::forward<Projection>(projection));
    }

    template <class Range, class Predicate, class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<lomuto_partition_fn, Range, Predicate, Projection> and
             ranges::forward_range<Range> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>> and
             not ranges::viewable_range<Range>)
    static constexpr auto operator()(Range range,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(
            noexcept(_lomuto_partition::algorithm(ranges::begin(range),
                                                  ranges::end(range),
                                                  std::forward<Predicate>(predicate),
                                                  std::forward<Projection>(projection))))
            -> std::pair<ranges::iterator_t<Range>, Range>
    {
        return {_lomuto_partition::algorithm(ranges::begin(range),
                                             ranges::end(range),
                                             std::forward<Predicate>(predicate),
                                             std::forward<Projection>(projection)),
                std::move(range)};
    }

    template <class Iter,
              class Sentinel,
              class Predicate,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<lomuto_partition_fn,
                                       Iter,
                                       Sentinel,
                                       Predicate,
                                       Projection> and
             ranges::forward_iterator<std::remove_cvref_t<Iter>> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Iter>> and
             ranges::indirect_unary_predicate<Predicate,
                                              ranges::projected<Iter, Projection>>)
    static constexpr auto operator()(Iter&& first,
                                     Sentinel&& last,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(
            noexcept(_lomuto_partition::algorithm(std::forward<Iter>(first),
                                                  std::forward<Sentinel>(last),
                                                  std::forward<Predicate>(predicate),
                                                  std::forward<Projection>(projection))))
            -> decltype(_lomuto_partition::algorithm(
                std::forward<Iter>(first),
                std::forward<Sentinel>(last),
                std::forward<Predicate>(predicate),
                std::forward<Projection>(projection)))
    {
        return _lomuto_partition::algorithm(std::forward<Iter>(first),
                                            std::forward<Sentinel>(last),
                                            std::forward<Predicate>(predicate),
                                            std::forward<Projection>(projection));
    }

} lomuto_partition;

namespace _lomuto_partition
{

template <class Predicate, class Projection>
struct _adapter<Predicate, Projection>::type
{
    Predicate pred_;
    Projection proj_;

private:
    template <class Range, class Adapter>
    requires std::same_as<std::remove_cvref_t<Adapter>, type>
    constexpr friend auto operator|(Range&& range, Adapter&& self)
        noexcept(noexcept(lomuto_partition(std::forward<Range>(range),
                                           std::forward<Predicate>(self.pred_),
                                           std::forward<Projection>(self.proj_))))
            -> decltype(lomuto_partition(std::forward<Range>(range),
                                         std::forward<Predicate>(self.pred_),
                                         std::forward<Projection>(self.proj_)))
    {
        return lomuto_partition(std::forward<Range>(range),
                                std::forward<Predicate>(self.pred_),
                                std::forward<Projection>(self.proj_));
    }
};

} // namespace _lomuto_partition

// Branchless Lomuto partition
// https://orlp.net/blog/branchless-lomuto-partitioning/
namespace _branchless_lomuto_partition
{

constexpr auto algorithm(auto start, auto end, auto&& predicate, auto&& projection)
{
    if (std::distance(start, end) == 0) {
        return start;
    }

    auto i = start;
    auto j = start;
    auto temp = ranges::iter_move(i);

    const auto last =
        i + std::iter_difference_t<decltype(start)>(ranges::distance(start, end) - 1);

    while (j != last) {
        *j = ranges::iter_move(i);
        ++j;
        *i = ranges::iter_move(j);
        i += predicate(projection(*i));
    }

    *j = ranges::iter_move(i);
    *i = std::move(temp);
    i += predicate(projection(*i));

    return i;
}

template <class Predicate, class Projection>
struct _adapter
{
    struct type;
};

template <class Predicate, class Projection>
using adapter = _adapter<Predicate, Projection>::type;

} // namespace _branchless_lomuto_partition

inline constexpr struct branchless_lomuto_fn
{
    template <class Predicate, class Projection = std::identity>
    requires(!ranges::range<Predicate> and !ranges::range<Projection>)
    static constexpr auto operator()(Predicate&& predicate, Projection&& projection = {})
        noexcept(std::is_nothrow_constructible_v<
                 _branchless_lomuto_partition::adapter<Predicate, Projection>>)
            -> _branchless_lomuto_partition::adapter<Predicate, Projection>
    {
        return {std::forward<Predicate>(predicate), std::forward<Projection>(projection)};
    }

    template <ranges::random_access_range Range,
              class Predicate,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<branchless_lomuto_fn, Range, Predicate, Projection> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return tag_invoke(branchless_lomuto_fn{},
                          std::forward<Range>(range),
                          std::forward<Predicate>(predicate),
                          std::forward<Projection>(projection));
    }

    template <class Itr,
              class Sentinel,
              class Predicate,
              class Projection = ranges::identity>
    requires(
        unifex::
            tag_invocable<branchless_lomuto_fn, Itr, Sentinel, Predicate, Projection> and
        ranges::sentinel_for<Sentinel, Itr> and
        ranges::indirect_unary_predicate<Predicate, ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& start,
                                     Sentinel&& end,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
    {
        return tag_invoke(branchless_lomuto_fn{},
                          std::forward<Itr>(start),
                          std::forward<Sentinel>(end),
                          std::forward<Predicate>(predicate),
                          std::forward<Projection>(projection));
    }

    template <ranges::random_access_range Range, class Predicate, class Projection>
    requires(not unifex::
                 tag_invocable<branchless_lomuto_fn, Range, Predicate, Projection> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = std::identity{})
        noexcept(noexcept(std::pair(
            _branchless_lomuto_partition::algorithm(ranges::begin(range),
                                                    ranges::end(range),
                                                    std::forward<Predicate>(predicate),
                                                    std::forward<Projection>(projection)),
            std::forward<Range>(range)))) -> std::pair<ranges::iterator_t<Range>, Range>
    {
        return {
            _branchless_lomuto_partition::algorithm(ranges::begin(range),
                                                    ranges::end(range),
                                                    std::forward<Predicate>(predicate),
                                                    std::forward<Projection>(projection)),
            std::forward<Range>(range)};
    }

    template <class Itr,
              class Sentinel,
              class Predicate,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<branchless_lomuto_fn,
                                       Itr,
                                       Sentinel,
                                       Predicate,
                                       Projection> and
             ranges::indirect_unary_predicate<Predicate,
                                              ranges::projected<Itr, Projection>> and
             ranges::random_access_iterator<std::remove_cvref_t<Itr>> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Itr>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& end,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(noexcept(_branchless_lomuto_partition::algorithm(
            std::forward<Itr>(first),
            std::forward<Sentinel>(end),
            std::forward<Predicate>(predicate),
            std::forward<Projection>(projection)))) -> std::remove_cvref_t<Itr>
    {
        return _branchless_lomuto_partition::algorithm(
            std::forward<Itr>(first),
            std::forward<Sentinel>(end),
            std::forward<Predicate>(predicate),
            std::forward<Projection>(projection));
    }
} branchless_lomuto_partition;

namespace _branchless_lomuto_partition
{

template <class Predicate, class Projection>
struct _adapter<Predicate, Projection>::type
{
    Predicate pred_;
    Projection proj_;

    template <class Adapter, ranges::random_access_range Range>
    requires std::same_as<std::remove_cvref_t<Adapter>, type>
    constexpr friend auto operator|(Range&& range, Adapter&& self) noexcept(
        noexcept(branchless_lomuto_partition(std::forward<Range>(range),
                                             std::forward<Predicate>(self.pred_),
                                             std::forward<Projection>(self.proj_))))
        -> decltype(branchless_lomuto_partition(std::forward<Range>(range),
                                                std::forward<Predicate>(self.pred_),
                                                std::forward<Projection>(self.proj_)))
    {
        return branchless_lomuto_partition(std::forward<Range>(range),
                                           std::forward<Predicate>(self.pred_),
                                           std::forward<Projection>(self.proj_));
    }
};

} // namespace _branchless_lomuto_partition
} // namespace algo

#include <algo/prologue.hpp>
