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

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/range/concepts.hpp>
#include <range/v3/range_concepts.hpp>
#include <type_traits>
#include <unifex/tag_invoke.hpp>

namespace algo
{

namespace _insertion_sort
{

template <class T>
T&& move_or_copy(T&& val)
{
    if constexpr (std::is_nothrow_move_assignable_v<std::remove_cvref_t<T>>
                  // don't move word size things
                  and not std::is_integral_v<std::remove_cvref_t<T>> and
                  not std::is_pointer_v<std::remove_cvref_t<T>>) {
        return static_cast<std::remove_reference_t<T>&&>(val);
    }
    else {
        return std::forward<T>(val);
    }
}

/**
 * @brief Implements insertion sort for a range satisfying `forward
 * iterable`.
 *
 * @param rng A range at least satisfying `std::ranges::forward_range`
 * @param cmp A comparison function. Use less for ascending order and
 * greater for descending order.
 */
template <class Itr, class Sentinel, class Relation, class Projection>
requires(ranges::forward_iterator<Itr> and
         !(ranges::bidirectional_iterator<Itr> or ranges::random_access_iterator<Itr>) and
         ranges::sentinel_for<Sentinel, Itr> and
         ranges::indirect_strict_weak_order<Relation,
                                            ranges::projected<Itr, Projection>,
                                            ranges::projected<Itr, Projection>>)
void algorithm(Itr start,
               Sentinel end,
               Relation const& relation,
               Projection const& projection)
    noexcept(std::is_nothrow_swappable_v<std::iter_value_t<Itr>>)
{
    using std::swap;
    if (start == end) {
        return;
    }

    // assume range of size 1 on left is sorted
    auto key_itr = start;
    ++key_itr;

    // TODO: Document loop invariants.
    for (; key_itr != end; ++key_itr) {
        // use first element of right range as key to sort into left range

        // find position of key in left range
        auto i = start;
        while (relation(projection(*key_itr), projection(*i)) and i != key_itr) {
            ++i;
        }

        // shift elements from i to key_iter by one
        // TODO: if RNG is contiguous and value_type is trivially
        // relocatable then use memcpy
        if (i != key_itr) {
            auto j = i;
            do { // NOLINT
                ++j;
                ranges::iter_swap(i, j);
            } while (j != key_itr);
        }

        // insert key into i
        ranges::iter_swap(key_itr, i);
    }
}

template <class Itr, class Sentinel, class Relation, class Projection>
requires(ranges::bidirectional_iterator<std::remove_cvref_t<Itr>> and
         ranges::sentinel_for<std::remove_cvref_t<Sentinel>, std::remove_cvref_t<Itr>> and
         ranges::indirect_strict_weak_order<Relation,
                                   ranges::projected<Itr, Projection>,
                                   ranges::projected<Itr, Projection>>)
void algorithm(Itr start,
               Sentinel end,
               Relation const& relation,
               Projection const& projection)
    noexcept(std::is_nothrow_swappable_v<std::iter_value_t<Itr>>)
{
    if (start == end) {
        return;
    }
    auto key_itr=start;
    ++key_itr;
    for (; key_itr != end; ++key_itr) {
        auto key{move_or_copy(*key_itr)};
        auto j = key_itr;
        auto i = j;
        --i;
        for (; j != start and relation(projection(key), projection(*i));
             --i, --j) {
            *j = move_or_copy(*i);
        }

        *j = move_or_copy(key);
    }
}

template <class Relation, class Projection>
struct _adapter
{
    struct type;
};

template <class Ordering, class Projection>
using adapter = _adapter<std::decay_t<Ordering>, std::decay_t<Projection>>::type;

} // namespace _insertion_sort

inline constexpr struct insertion_sort_fn
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(!ranges::range<Relation> and !ranges::range<Projection>)
    static constexpr auto operator()(Relation&& ordering = {},
                                     Projection&& projection = {})
        -> _insertion_sort::adapter<Relation, Projection>
    {
        return {std::forward<Relation>(ordering), std::forward<Projection>(projection)};
    }

    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<insertion_sort_fn, Range, Relation, Projection>
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<insertion_sort_fn,
                                                    decltype(range),
                                                    Relation,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<insertion_sort_fn, Range, Relation, Projection>
    {
        return tag_invoke(insertion_sort_fn{},
                          std::forward<Range>(range),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<insertion_sort_fn, Itr, Sentinel, Relation, Projection>
    static constexpr auto operator()(
        Itr&& first,
        Sentinel&& last,
        Relation&& relation = {},
        Projection&& projection =
            {}) noexcept(unifex::is_nothrow_tag_invocable_v<insertion_sort_fn,
                                                            Itr,
                                                            Sentinel,
                                                            Relation,
                                                            Projection>)
        -> unifex::
            tag_invoke_result_t<insertion_sort_fn, Itr, Sentinel, Relation, Projection>
    {
        return tag_invoke(insertion_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<insertion_sort_fn, Range, Relation, Projection> and
             ranges::range<Range> and ranges::sized_range<Range> and
             ranges::forward_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(
            _insertion_sort::algorithm(begin(range), end(range), relation, projection)))
            -> sorted<Range, Relation, Projection>
    {
        _insertion_sort::algorithm(
            ranges::begin(range), ranges::end(range), relation, projection);
        return {std::forward<Range>(range),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }

    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<insertion_sort_fn, Itr, Sentinel, Relation, Projection> and
             ranges::forward_iterator<std::remove_cvref_t<Itr>> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Itr>> and
             ranges::indirect_strict_weak_order<Relation,
                                                ranges::projected<Itr, Projection>,
                                                ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(_insertion_sort::algorithm(first, last, relation, projection)))
            -> sorted<Itr, Sentinel, Relation, Projection>
    {
        _insertion_sort::algorithm(first, last, relation, projection);
        return {std::forward<Itr>(first),
                std::forward<Sentinel>(last),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }
} insertion_sort;

namespace _insertion_sort
{

template <class Relation, class Projection>
struct _adapter<Relation, Projection>::type
{
    [[no_unique_address]] Relation relation_;     // NOLINT
    [[no_unique_address]] Projection projection_; //

private:
    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(Range&& range, Adapter&& self)
        noexcept(noexcept(insertion_sort(std::forward<Range>(range),
                                         std::forward_like<Adapter>(self.relation_),
                                         std::forward_like<Adapter>(self.projection_))))
            -> decltype(insertion_sort(std::forward<Range>(range),
                                       std::forward_like<Adapter>(self.relation_),
                                       std::forward_like<Adapter>(self.projection_)))
    {
        return insertion_sort(std::forward<Range>(range),
                              std::forward_like<Adapter>(self.relation_),
                              std::forward_like<Adapter>(self.projection_));
    }
};

} // namespace _insertion_sort
inline constexpr auto insertion_sort_descending = insertion_sort(ordering::descending);
inline constexpr auto insertion_sort_ascending = insertion_sort(ordering::ascending);

} // namespace algo
