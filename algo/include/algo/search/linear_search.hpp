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

#include <range/v3/range_concepts.hpp>
#include <unifex/tag_invoke.hpp>

/** AI explanation
 *
 * This C++ code implements a linear search algorithm and provides an interface to use it
 * with range-v3, the standard library's extension for ranges and algorithms.
 *
 * The main component is the `algorithm` function template that performs the actual linear
 * search operation on any iterable container of elements (ranges::forward_range or raw
 * arrays). It takes a key to search for within the provided range and returns an iterator
 * pointing at the first occurrence of this key in the range, if found.
 *
 * The code also includes concepts and requirements checks using C++20 features like
 * concept lite syntax `std::forward_iterator`, `ranges::sentinel_for` etc., that ensure
 * type safety.
 *
 * To use it with different ranges (like containers or views in the range-v3 library), a
 * specific adapter class template named `_adapter` is created, which wraps around a key
 * to be searched for and provides an overloaded `operator|` for pipeline usage.
 *
 * The functionality of linear search can also be invoked directly with iterators using
 * `tag_invoke` from unifex library. This allows it to be used as a callable object in the
 * same way as many other range-v3 algorithms or views. It is useful when you want more
 * flexibility and control over how your algorithm is being called (like passing custom
 * comparison functions).
 */

namespace algo
{
namespace _linear_search
{

template <std::forward_iterator Itr,
          ranges::sentinel_for<Itr> Sentinel,
          class CMP = std::equal_to<>>
auto algorithm(Itr start,
               Sentinel end,
               std::iter_value_t<Itr> const& key,
               CMP cmp = CMP{}) -> Itr
{
    while (start != end) {
        if (cmp(*start, key)) {
            break;
        }
        ++start;
    }
    return start;
}

/**
 * `_adapter` is a class template that provides an adapter for linear search algorithms.
 * It has a nested `type` struct that defines the actual adapter type. This adapter type
 * can be used with the `|` operator to pipe a range into a linear search algorithm.
 */
template <class Key>
struct _adapter final
{
    struct type;
};

template <class Key>
using adapter = _adapter<Key>::type;

/**
 * `_cpo` is a namespace that contains a struct called `_fn`, which is a tag-invocable
 * object for linear search algorithms. This struct has overloaded `operator()` functions
 * that take different arguments, including ranges and iterators. The overloads of
 * `operator()` are used to provide the linear search algorithm functionality.
 */
namespace _cpo
{
inline constexpr struct _fn
{

    // TODO: Better refinement
    template <class Key>
    requires(!(ranges::range<Key> or ranges::view_<Key>))
    static constexpr auto operator()(Key&& key)
    {
        return adapter<Key>{std::forward<Key>(key)};
    }

    template <ranges::forward_range Range, class Key>
    requires(unifex::tag_invocable<_fn, Range, Key> and ranges::viewable_range<Range>)
    static constexpr auto operator()(Range&& range, Key&& key)
    {
        return tag_invoke(_fn{}, std::forward<Range>(range), std::forward<Key>(key));
    }
    template <std::forward_iterator Itr, ranges::sentinel_for<Itr> Sentinel, class Key>
    requires(unifex::tag_invocable<_fn, Itr, Sentinel, Key>)
    static constexpr auto operator()(Itr&& first, Sentinel&& end, Key&& key)
    {
        return tag_invoke(_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(end),
                          std::forward<Key>(key));
    }

private:
    template <ranges::forward_range Range, class Key>
    requires(ranges::viewable_range<Range>)
    friend constexpr auto tag_invoke([[maybe_unused]] _fn const& /*unused*/,
                                     Range&& range,
                                     Key&& key)
    {
        return algorithm(
            ranges::begin(range), ranges::end(range), std::forward<Key>(key));
    }

    template <std::forward_iterator Itr, ranges::sentinel_for<Itr> Sentinel, class Key>
    friend constexpr auto tag_invoke([[maybe_unused]] _fn const& /*unused*/,
                                     Itr&& first,
                                     Sentinel&& end,
                                     Key&& key)
    {
        return algorithm(std::forward<Itr>(first),
                         std::forward<Sentinel>(end),
                         std::forward<Key>(key));
    }
} linear_search;
} // namespace _cpo

} // namespace _linear_search

/**
 * `linear_search` is an alias for `_cpo::linear_search`, which is a tag-invocable object
 * for linear search algorithms. This object can be used with the `|` operator to pipe a
 * range into a linear search algorithm.
 */
using _linear_search::_cpo::linear_search;

namespace _linear_search
{

template <class Key>
struct _adapter<Key>::type final
{
    Key key_;

    template <ranges::forward_range Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type> and
             ranges::viewable_range<Range>)
    friend constexpr auto operator|(Range&& range, Adapter&& self)
    {
        return linear_search(std::forward<Range>(range),
                             std::forward_like<Adapter>(self.key_));
    }
};

} // namespace _linear_search

} // namespace algo
