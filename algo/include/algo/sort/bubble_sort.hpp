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

#include <range/v3/iterator/operations.hpp>
#include <range/v3/range_concepts.hpp>

#include "ordering.hpp"
#include "sorted.hpp"
#include <unifex/tag_invoke.hpp>

#include <algo/prelude.hpp>

namespace algo
{
namespace _bubble_sort
{

template <class Itr, class Sentinel, class Relation, class Projection>
constexpr void algorithm(Itr&& first,
                         Sentinel&& last,
                         Relation&& relation,
                         Projection&& projection)
{
    ranges::iter_difference_t<std::remove_cvref_t<Itr>> new_n = 0;

    for (auto size = ranges::distance(first, last); size > 0; size = new_n) {
        new_n = 0;
        auto a = first;
        auto b = next(first);
        for (auto i = new_n + 1; i < size; ++i, ++a, ++b) {
            if (relation(projection(*b), projection(*a))) {
                ranges::iter_swap(a, b);
                new_n = i;
            }
        }
        size = new_n;
    }
}

template <class Relation, class Projection>
struct _adapter final
{
    struct type;
};

template <class Relation, class Projection>
using adapter = _adapter<Relation, Projection>::type;

} // namespace _bubble_sort

inline constexpr struct bubble_sort_fn final
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(!ranges::range<Relation> and !ranges::range<Projection>)
    static constexpr auto operator()(Relation&& relation = {},
                                     Projection&& projection = {})
        -> _bubble_sort::adapter<Relation, Projection>
    {
        return {std::forward<Relation>(relation), std::forward<Projection>(projection)};
    }

    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(unifex::tag_invocable<bubble_sort_fn, Range, Relation, Projection>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(
            unifex::
                is_nothrow_tag_invocable_v<bubble_sort_fn, Range, Relation, Projection>)
            -> unifex::tag_invoke_result_t<bubble_sort_fn, Range, Relation, Projection>
    {
        return tag_invoke(bubble_sort_fn{},
                          std::forward<Range>(range),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(unifex::tag_invocable<bubble_sort_fn, Itr, Sentinel, Relation, Projection>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<bubble_sort_fn,
                                                    Itr,
                                                    Sentinel,
                                                    Relation,
                                                    Projection>)
            -> unifex::
                tag_invoke_result_t<bubble_sort_fn, Itr, Sentinel, Relation, Projection>
    {
        return tag_invoke(bubble_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Range,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<bubble_sort_fn, Range, Relation, Projection> and
             ranges::forward_range<Range> and ranges::sized_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        -> sorted<Range, Relation, Projection>
    {
        _bubble_sort::algorithm(
            ranges::begin(range), ranges::end(range), relation, projection);
        return {std::forward<Range>(range),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }

    template <class Itr,
              class Sentinel,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<bubble_sort_fn, Itr, Sentinel, Relation, Projection> and
             ranges::forward_iterator<Itr> and ranges::sentinel_for<Sentinel, Itr> and
             ranges::indirect_strict_weak_order<Relation,
                                                ranges::projected<Itr, Projection>,
                                                ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        -> sorted<ranges::subrange<Itr, Sentinel>, Relation, Projection>
    {
        _bubble_sort::algorithm(first, last, relation, projection);
        return {ranges::subrange(std::forward<Itr>(first), std::forward<Sentinel>(last)),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }
} bubble_sort;

namespace _bubble_sort
{
template <class Relation, class Projection>
struct _adapter<Relation, Projection>::type final
{
    [[no_unique_address]] Relation relation;     // NOLINT
    [[no_unique_address]] Projection projection; // NOLINT

    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(Range&& range, Adapter&& adapter)
        noexcept(noexcept(bubble_sort(std::forward<Range>(range),
                                      std::forward_like<Adapter>(adapter.relation),
                                      std::forward_like<Adapter>(adapter.projection))))
            -> decltype(bubble_sort(std::forward<Range>(range),
                                    std::forward_like<Adapter>(adapter.relation),
                                    std::forward_like<Adapter>(adapter.projection)))
    {
        return bubble_sort(std::forward<Range>(range),
                           std::forward_like<Adapter>(adapter.relation),
                           std::forward_like<Adapter>(adapter.projection));
    }
};

} // namespace _bubble_sort

constexpr auto bubble_sort_decending = bubble_sort(ordering::descending);
constexpr auto bubble_sort_ascending = bubble_sort(ordering::ascending);

} // namespace algo

#include <algo/prologue.hpp>
