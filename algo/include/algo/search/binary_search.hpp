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

#include "algo/sort/sorted.hpp"
#include <algo/sort/ordering.hpp>

#include <range/v3/range.hpp>

#include <algo/prelude.hpp>
#include <range/v3/view/common.hpp>
#include <unifex/tag_invoke.hpp>

namespace algo
{

namespace _binary_search
{

/**
 * @brief Binary search algorithm.
 *
 * This code is an implementation of a binary search algorithm, specifically
 * for sorted ranges. It defines an `algorithm` function that takes a sorted range and an
 * element to search for, and returns the iterator to the element if it is found in the
 * range. If the element is not found, it returns an iterator to the insertion point
 * (i.e., where the element should be inserted into the sorted range).
 *
 * Preconditions
 *  - A sorted range.
 *
 * TODO: Don't take a key, take a unary function and construct that function from the
 * binary_search object's operator().
 *
 * @param rng a forward range
 * @param key the element to search for
 * @param cmp a comparison function
 * @return an iterator to the element, or nullopt
 */
template <std::random_access_iterator Itr,
          ranges::sentinel_for<Itr> Sentinel,
          class Key,
          class Relation,
          class Projection>
auto algorithm(
    Itr front, Sentinel back, Key const& key, Relation&& cmp, Projection&& proj) -> Itr
{
    auto dist = ranges::distance(front, back) - 1;
    auto last = front + dist;

    while (front != last) {
        dist /= 2;
        auto mitr = front + dist;
        if (cmp(proj(*mitr), proj(key))) {
            front = mitr;
            ++front;
        }
        else {
            last = mitr;
        }
    }
    if (front == last && cmp(proj(*front), proj(key))) {
        ++front;
    }

    return front;
}

} // namespace _binary_search

/**
 * `binary_search_fn` is a tag-invocable object for binary search algorithms.
 * This object can be used with the `|` operator to pipe a range into a
 * binary search algorithm.
 */
constexpr struct binary_search_fn
{
    template <ranges::forward_range Range,
              class Key,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(unifex::tag_invocable<binary_search_fn, Range, Key, Relation, Projection>)
    static constexpr auto operator()(Range&& range,
                                     Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {}) //
        noexcept(unifex::is_nothrow_tag_invocable_v<binary_search_fn,
                                                    Range,
                                                    Key,
                                                    Relation,
                                                    Projection>)
            -> unifex::
                tag_invoke_result_t<binary_search_fn, Range, Key, Relation, Projection>
    {
        return unifex::tag_invoke(binary_search_fn{},
                                  std::forward<Range>(range),
                                  std::forward<Key>(key),
                                  std::forward<Relation>(rel),
                                  std::forward<Projection>(proj));
    }

    template <std::forward_iterator Itr,
              ranges::sentinel_for<Itr> Sentinel,
              class Key,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(unifex::tag_invocable<binary_search_fn,
                                   Itr,
                                   Sentinel,
                                   Key,
                                   Relation,
                                   Projection>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& end,
                                     Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<binary_search_fn,
                                                    Itr,
                                                    Sentinel,
                                                    Key,
                                                    Relation,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<binary_search_fn,
                                           Itr,
                                           Sentinel,
                                           Key,
                                           Relation,
                                           Projection>
    {
        return tag_invoke(binary_search_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(end),
                          std::forward<Key>(key),
                          std::forward<Relation>(rel),
                          std::forward<Projection>(proj));
    }

    template <class Range,
              class Key,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<binary_search_fn, Range, Key, Relation, Projection> and
             not Sorted<Range> and ranges::random_access_range<Range> and
             ranges::sized_range<Range> and ranges::viewable_range<Range>)
    static constexpr auto operator()(Range&& range,
                                     Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {})
        noexcept(noexcept(_binary_search::algorithm(ranges::begin(range),
                                                    ranges::end(range),
                                                    std::forward<Key>(key),
                                                    std::forward<Relation>(rel),
                                                    std::forward<Projection>(proj))))
            -> decltype(_binary_search::algorithm(ranges::begin(range),
                                                  ranges::end(range),
                                                  std::forward<Key>(key),
                                                  std::forward<Relation>(rel),
                                                  std::forward<Projection>(proj)))
    {
        return _binary_search::algorithm(ranges::begin(range),
                                         ranges::end(range),
                                         std::forward<Key>(key),
                                         std::forward<Relation>(rel),
                                         std::forward<Projection>(proj));
    }

    template <Sorted Range, class Key>
    constexpr auto operator()(Range&& range, Key&& key) const noexcept(noexcept((*this)(
        range.base(), std::forward<Key>(key), range.relation(), range.projection())))
        -> decltype((*this)(
            range.base(), std::forward<Key>(key), range.relation(), range.projection()))
    {
        return (*this)(
            range.base(), std::forward<Key>(key), range.relation(), range.projection());
    }

    template <class Itr,
              class Sentinel,
              class Key,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<binary_search_fn, Itr, Sentinel, Relation, Projection> and
             ranges::random_access_iterator<Itr> and ranges::sentinel_for<Sentinel, Itr>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& end,
                                     Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {})
        noexcept(noexcept(_binary_search::algorithm(std::forward<Itr>(first),
                                                    std::forward<Sentinel>(end),
                                                    std::forward<Key>(key),
                                                    std::forward<Relation>(rel),
                                                    std::forward<Projection>(proj))))
            -> decltype(_binary_search::algorithm(std::forward<Itr>(first),
                                                  std::forward<Sentinel>(end),
                                                  std::forward<Key>(key),
                                                  std::forward<Relation>(rel),
                                                  std::forward<Projection>(proj)))
    {
        return _binary_search::algorithm(std::forward<Itr>(first),
                                         std::forward<Sentinel>(end),
                                         std::forward<Key>(key),
                                         std::forward<Relation>(rel),
                                         std::forward<Projection>(proj));
    }
} binary_search;

} // namespace algo

#include <algo/prologue.hpp>
