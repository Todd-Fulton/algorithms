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
template <std::forward_iterator Itr,
          ranges::sentinel_for<Itr> Sentinel,
          class CMP = std::less<>>
auto algorithm(Itr start,
               Sentinel end,
               std::iter_value_t<Itr> const& key,
               CMP cmp = CMP{}) -> Itr
{
    using std::distance;

    Itr last = start;
    std::iter_difference_t<Itr> dist{0};
    if constexpr (std::random_access_iterator<Itr>) {
        dist = std::iter_difference_t<Itr>(distance(start, end)) - 1;
        last += dist;
    }
    else {
        auto cursor = last;
        while (cursor != end) {
            last = cursor;
            ++cursor;
            ++dist;
        }
    }

    while (start != last) {
        Itr mitr;
        dist /= 2;
        if constexpr (std::random_access_iterator<Itr>) {
            mitr = start + dist;
        }
        else {
            mitr = start;
            auto mid = dist;
            while (mid) {
                ++mitr;
                --mid;
            }
        }
        if (cmp(*mitr, key)) {
            start = mitr;
            ++start;
        }
        else {
            last = mitr;
        }
    }
    if (start == last && cmp(*start, key)) {
        ++start;
    }

    return start;
}

// TODO: Dont use ordering, use a custom type for searching that
// represents if we should insert before or after

/**
 * `_adapter` is a class template that provides an adapter for binary search algorithms.
 * It has a nested `type` struct that defines the actual adapter type. This adapter type
 * can be used with the `|` operator to pipe a range into a binary search algorithm.
 */
template <class Key, class Ordering>
struct _adapter final
{
    struct type;
};

template <class Key, class Ordering>
using adapter = _adapter<Key, std::remove_cvref_t<Ordering>>::type;

/**
 * `_cpo` is a namespace that contains a struct called `_fn`, which is a tag-invocable
 * object for binary search algorithms. This struct has overloaded `operator()` functions
 * that take different arguments, including ranges and iterators. The overloads of
 * `operator()` are used to provide the binary search algorithm functionality.
 */
namespace _cpo
{
constexpr struct _fn
{

    // TODO: Better refinement
    template <class Key, class Ordering>
    requires(!(ranges::range<Key> or ranges::view_<Key>))
    static constexpr auto operator()(Key&& key, Ordering&& ord)
    {
        return adapter<Key, Ordering>{std::forward<Key>(key),
                                      std::forward<Ordering>(ord)};
    }

    template <ranges::forward_range Range,
              class Key,
              class Ordering = ordering::ascending>
    requires(unifex::tag_invocable<_fn, Range, Key, Ordering> and
             ranges::viewable_range<Range>)
    static constexpr auto operator()(Range&& range, Key&& key, Ordering&& ordering = {})
    {
        return tag_invoke(_fn{},
                          std::forward<Range>(range),
                          std::forward<Key>(key),
                          std::forward<Ordering>(ordering));
    }
    template <std::forward_iterator Itr,
              ranges::sentinel_for<Itr> Sentinel,
              class Key,
              class Ordering = ordering::ascending>
    requires(unifex::tag_invocable<_fn, Itr, Sentinel, Key, Ordering>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& end,
                                     Key&& key,
                                     Ordering&& ordering = {})
    {
        return tag_invoke(_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(end),
                          std::forward<Key>(key),
                          std::forward<Ordering>(ordering));
    }

private:
    template <ranges::forward_range Range, class Key, class Ordering>
    requires(ranges::viewable_range<Range> and ranges::sized_range<Range>)
    friend constexpr auto tag_invoke([[maybe_unused]] _fn const& /*unused*/,
                                     Range&& range,
                                     Key&& key,
                                     [[maybe_unused]] Ordering const& /*ordering*/)
    {
        if constexpr (ranges::common_range<Range>) {
            return algorithm(ranges::begin(range),
                             ranges::end(range),
                             std::forward<Key>(key),
                             predicate_for_t<Ordering, RNG_VALUE_T(range)>{});
        }
        else {

            auto vi = ranges::views::common(std::forward<Range>(range));
            return algorithm(ranges::begin(vi),
                             ranges::end(vi),
                             std::forward<Key>(key),
                             predicate_for_t<Ordering, RNG_VALUE_T(range)>{});
        }
    }

    template <std::forward_iterator Itr,
              ranges::sentinel_for<Itr> Sentinel,
              class Key,
              class Ordering>
    friend constexpr auto tag_invoke([[maybe_unused]] _fn const& /*unused*/,
                                     Itr&& first,
                                     Sentinel&& end,
                                     Key&& key,
                                     [[maybe_unused]] Ordering const& /*ordering*/)
    {
        return algorithm(std::forward<Itr>(first),
                         std::forward<Sentinel>(end),
                         std::forward<Key>(key),
                         predicate_for_t<Ordering, ITR_VALUE_T(first)>{});
    }
} binary_search;
} // namespace _cpo

} // namespace _binary_search

/**
 * `binary_search` is an alias for `_cpo::binary_search`, which is a tag-invocable object
 * for binary search algorithms. This object can be used with the `|` operator to pipe a
 * range into a binary search algorithm.
 */
using _binary_search::_cpo::binary_search;

namespace _binary_search
{

template <class Key, class Ordering>
struct _adapter<Key, Ordering>::type final
{
    Key key_;

    template <ranges::forward_range Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type> and
             ranges::viewable_range<Range> and ranges::sized_range<Range>)
    friend constexpr auto operator|(Range&& range, Adapter&& self)
    {
        return binary_search(std::forward<Range>(range),
                             std::forward_like<Adapter>(self.key_),
                             Ordering{});
    }

    // TODO: Take a std::pair<Itr, Sent> as pipe?
    // TODO: Take a pipable as pipe and compose?
};

} // namespace _binary_search

} // namespace algo

#include <algo/prologue.hpp>
