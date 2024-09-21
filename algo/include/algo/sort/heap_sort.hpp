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
#include <range/v3/iterator/operations.hpp>
#include <range/v3/range/traits.hpp>

#include <unifex/tag_invoke.hpp>

namespace algo
{

namespace _heap_sort
{

constexpr auto i_parent(auto x) noexcept
{
    assert(x != 0);
    return (x - 1) / 2;
}

constexpr auto i_left_child(auto x) noexcept
{
    return (2 * x) + 1;
}

constexpr auto i_right_child(auto x) noexcept
{
    return (2 * x) + 2;
}

constexpr auto leaf_search(
    auto const& itr, auto i, auto end, auto const& relation, auto const& projection)
    noexcept(noexcept(relation(projection(*itr), projection(*itr))))
{
    auto j = i;
    auto lc = i_left_child(j);
    auto rc = lc + 1;
    while (rc < end) {
        if (!relation(projection(*(itr + rc)), projection(*(itr + lc)))) {
            j = rc;
        }
        else {
            j = lc;
        }
        lc = i_left_child(j);
        rc = lc + 1;
    }

    lc = i_left_child(j);
    if (lc < end) {
        j = lc;
    }
    return j;
}

constexpr void sift_down(
    auto& itr, auto i, auto end, auto const& relation, auto const& projection)
    noexcept(noexcept(ranges::iter_swap(itr, itr)) and
             noexcept(relation(projection(*itr), projection(*itr))))
{
    auto j = leaf_search(itr, i, end, relation, projection);
    auto i_itr = itr + i;
    while (!relation(projection(*i_itr), projection(*(itr + j))) and j != 0) {
        j = i_parent(j);
    }
    while (j > i) {
        ranges::iter_swap(i_itr, itr + j);
        j = i_parent(j);
    }
};

constexpr void heapify(auto& itr, auto size, auto const& relation, auto const& projection)
    noexcept(noexcept(sift_down(itr, 1, size, relation, projection)))
{
    auto start = i_parent(size + 1);

    while (start > 0) {
        --start;
        sift_down(itr, start, size, relation, projection);
    }
};

template <class I, ranges::sentinel_for<I> Sentinel, class Relation, class Projection>
constexpr void algorithm(I start,
                         Sentinel end,
                         Relation&& relation,
                         Projection&& projection)
    noexcept(noexcept(heapify(start, 1, relation, projection)) and
             noexcept(sift_down(start, 0, 1, relation, projection)))
{
    auto size = ranges::distance(start, end);
    heapify(start, size, relation, projection);

    while (size > 1) {
        --size;
        ranges::iter_swap(start + size, start);
        sift_down(start, 0, size, relation, projection);
    }
}

template <class Range, class Relation, class Projection>
constexpr auto algorithm(Range&& range, Relation&& relation, Projection&& projection)
    noexcept(noexcept(algorithm(
                 ranges::begin(range), ranges::end(range), relation, projection)) and
             std::is_nothrow_constructible_v<sorted<Range, Relation, Projection>,
                                             Range,
                                             Relation,
                                             Projection>)
        -> sorted<Range, Relation, Projection>
{
}

template <class Relation, class Projection>
struct _adapter
{
    struct type;
};

template <class Relation, class Projection>
using adapter = _adapter<std::decay_t<Relation>, std::decay_t<Projection>>::type;

} // namespace _heap_sort

inline constexpr struct heap_sort_fn
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(not ranges::range<Relation> and not ranges::range<Projection>)
    static constexpr auto operator()(Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(
            std::is_nothrow_constructible_v<_heap_sort::adapter<Relation, Projection>,
                                            Relation,
                                            Projection>)
            -> _heap_sort::adapter<Relation, Projection>
    {
        return {std::forward<Relation>(relation), std::forward<Projection>(projection)};
    }

    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(unifex::tag_invocable<heap_sort_fn, Range, Relation, Projection>)
    static constexpr auto operator()(Range&& rng,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(
            unifex::is_nothrow_tag_invocable_v<heap_sort_fn, Range, Relation, Projection>)
            -> unifex::tag_invoke_result_t<heap_sort_fn, Range, Relation, Projection>
    {
        return tag_invoke(heap_sort_fn{},
                          std::forward<Range>(rng),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(unifex::tag_invocable<heap_sort_fn, Itr, Sentinel, Relation, Projection>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {}) //
        noexcept(unifex::is_nothrow_tag_invocable_v<heap_sort_fn,
                                                    Itr,
                                                    Sentinel,
                                                    Relation,
                                                    Projection>)
            -> unifex::
                tag_invoke_result_t<heap_sort_fn, Itr, Sentinel, Relation, Projection>
    {
        return tag_invoke(heap_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    // TODO: reduce restriction on random_access_range if able
    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<heap_sort_fn, Range, Relation, Projection> and
             ranges::random_access_range<Range> and ranges::sized_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(_heap_sort::algorithm(
            ranges::begin(range), ranges::end(range), relation, projection)))
            -> sorted<Range, Relation, Projection>
    {
        _heap_sort::algorithm(
            ranges::begin(range), ranges::end(range), relation, projection);
        return {std::forward<Range>(range),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }

    // TODO: reduce restriction on random_access_range if able
    template <class Itr, class Sentinel, class Relation, class Projection>
    requires(not unifex::
                 tag_invocable<heap_sort_fn, Itr, Sentinel, Relation, Projection> and
             ranges::random_access_iterator<std::remove_cvref_t<Itr>> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Itr>> and
             ranges::indirect_strict_weak_order<Relation,
                                                ranges::projected<Itr, Projection>,
                                                ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation,
                                     Projection&& projection)
        noexcept(noexcept(_heap_sort::algorithm(first, last, relation, projection)))
            -> sorted<Itr, Sentinel, Relation, Projection>
    {
        _heap_sort::algorithm(first, last, relation, projection);
        return {std::forward<Itr>(first),
                std::forward<Sentinel>(last),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }
} heap_sort;

namespace _heap_sort
{
template <class Relation, class Projection>
struct _adapter<Relation, Projection>::type final
{
    [[no_unique_address]] Relation relation;
    [[no_unique_address]] Projection projection;

    template <class Range, class Adapter>
    requires(ranges::random_access_range<Range> and
             std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(Range&& range, Adapter&& adapter)
    {
        return heap_sort(std::forward<Range>(range),
                         std::forward_like<Adapter>(adapter.relation),
                         std::forward_like<Adapter>(adapter.projection));
    }
};

} // namespace _heap_sort

} // namespace algo
