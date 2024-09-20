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

#include "algo/sort/ordering.hpp"
#include "algo/sort/sorted.hpp"
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
          ranges::sentinel_for<std::remove_cvref_t<Itr>> Sentinel,
          class Relation,
          class Projection>
auto algorithm(Itr start,
               Sentinel end,
               std::iter_value_t<Itr> const& key,
               Relation&& rel,
               Projection&& proj) -> Itr
{
    while (start != end) {
        if (!rel(proj(*start), proj(key))) {
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
template <class Key, class Relation, class Projection>
struct _adapter final
{
    struct type;
};

template <class Key, class Relation, class Projection>
using adapter = _adapter<Key, std::decay_t<Relation>, std::decay_t<Projection>>::type;

/**
 * `_cpo` is a namespace that contains a struct called `_fn`, which is a tag-invocable
 * object for linear search algorithms. This struct has overloaded `operator()` functions
 * that take different arguments, including ranges and iterators. The overloads of
 * `operator()` are used to provide the linear search algorithm functionality.
 */
namespace _cpo
{
} // namespace _cpo

} // namespace _linear_search

/**
 * `linear_search_fn` is a tag-invocable object
 * for linear search algorithms. This object can be used with the `|` operator to pipe a
 * range into a linear search algorithm.
 */
inline constexpr struct linear_search_fn
{

    template <class Key,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(!ranges::range<Key> and
             ranges::strict_weak_order<Relation,
                                       ranges::invoke_result_t<Projection, Key>,
                                       ranges::invoke_result_t<Projection, Key>>)
    static constexpr auto operator()(Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {})
        noexcept(std::is_nothrow_constructible_v<
                 _linear_search::adapter<Key, Relation, Projection>,
                 Key,
                 Relation,
                 Projection>) //
        -> _linear_search::adapter<Key, Relation, Projection>
    {
        return {std::forward<Key>(key),
                std::forward<Relation>(rel),
                std::forward<Projection>(proj)};
    }

    template <ranges::forward_range Range, class Key, class Relation, class Projection>
    requires(unifex::tag_invocable<linear_search_fn, Range, Key, Relation, Projection> and
             ranges::viewable_range<Range>)
    static constexpr auto operator()(Range&& range,
                                     Key&& key,
                                     Relation&& rel,
                                     Projection&& proj) //
        noexcept(unifex::is_nothrow_tag_invocable_v<linear_search_fn,
                                                    Range,
                                                    Key,
                                                    Relation,
                                                    Projection>)
            -> unifex::
                tag_invoke_result_t<linear_search_fn, Range, Key, Relation, Projection>
    {
        return tag_invoke(linear_search_fn{},
                          std::forward<Range>(range),
                          std::forward<Key>(key),
                          std::forward<Relation>(rel),
                          std::forward<Projection>(proj));
    }

    template <std::forward_iterator Itr,
              ranges::sentinel_for<std::remove_cvref_t<Itr>> Sentinel,
              class Key,
              class Relation,
              class Projection>
    requires(unifex::tag_invocable<linear_search_fn,
                                   Itr,
                                   Sentinel,
                                   Key,
                                   Relation,
                                   Projection>)
    static constexpr auto operator()(
        Itr&& first, Sentinel&& end, Key&& key, Relation&& rel, Projection&& proj)
        noexcept(unifex::is_nothrow_tag_invocable_v<linear_search_fn,
                                                    Itr,
                                                    Sentinel,
                                                    Key,
                                                    Relation,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<linear_search_fn,
                                           Itr,
                                           Sentinel,
                                           Key,
                                           Relation,
                                           Projection>
    {
        return tag_invoke(linear_search_fn{},
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
                 tag_invocable<linear_search_fn, Range, Key, Relation, Projection> and
             not Sorted<Range> and
             ranges::forward_range<Range> and ranges::viewable_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {})
        noexcept(noexcept(_linear_search::algorithm(ranges::begin(range),
                                                    ranges::end(range),
                                                    std::forward<Key>(key),
                                                    std::forward<Relation>(rel),
                                                    std::forward<Projection>(proj))))
            -> decltype(_linear_search::algorithm(ranges::begin(range),
                                                  ranges::end(range),
                                                  std::forward<Key>(key),
                                                  std::forward<Relation>(rel),
                                                  std::forward<Projection>(proj)))
    {
        return _linear_search::algorithm(ranges::begin(range),
                                         ranges::end(range),
                                         std::forward<Key>(key),
                                         std::forward<Relation>(rel),
                                         std::forward<Projection>(proj));
    }

    template <Sorted Range, class Key>
    requires(ranges::viewable_range<Range>)
    constexpr auto operator()(Range&& range, Key&& key) const noexcept(noexcept((*this)(
        range.base(), std::forward<Key>(key), range.relation(), range.projection())))
        -> decltype((*this)(
            range.base(), std::forward<Key>(key), range.relation(), range.projection()))
    {
        return (*this)(
            range.base(), std::forward<Key>(key), range.relation(), range.projection());
    }

    template <std::forward_iterator Itr,
              ranges::sentinel_for<std::remove_cvref_t<Itr>> Sentinel,
              class Key,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<linear_search_fn,
                                       Itr,
                                       Sentinel,
                                       Key,
                                       Relation,
                                       Projection> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<std::remove_cvref_t<Itr>, Projection>,
                 ranges::projected<std::remove_cvref_t<Itr>, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& end,
                                     Key&& key,
                                     Relation&& rel = {},
                                     Projection&& proj = {})
        noexcept(noexcept(_linear_search::algorithm(std::forward<Itr>(first),
                                                    std::forward<Sentinel>(end),
                                                    std::forward<Key>(key),
                                                    std::forward<Relation>(rel),
                                                    std::forward<Projection>(proj))))
            -> decltype(_linear_search::algorithm(std::forward<Itr>(first),
                                                  std::forward<Sentinel>(end),
                                                  std::forward<Key>(key),
                                                  std::forward<Relation>(rel),
                                                  std::forward<Projection>(proj)))
    {
        return _linear_search::algorithm(std::forward<Itr>(first),
                                         std::forward<Sentinel>(end),
                                         std::forward<Key>(key),
                                         std::forward<Relation>(rel),
                                         std::forward<Projection>(proj));
    }
} linear_search;

namespace _linear_search
{

template <class Key, class Relation, class Projection>
struct _adapter<Key, Relation, Projection>::type final
{
    [[no_unique_address]] Key key_;
    [[no_unique_address]] Relation rel_;
    [[no_unique_address]] Projection proj_;

    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type> and
             ranges::viewable_range<Range>)
    friend constexpr auto operator|(Range&& range, Adapter&& self)
    {
        return linear_search(std::forward<Range>(range),
                             std::forward_like<Adapter>(self.key_),
                             std::forward_like<Adapter>(self.rel_),
                             std::forward_like<Adapter>(self.proj_));
    }
};

} // namespace _linear_search

} // namespace algo
