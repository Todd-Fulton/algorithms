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

#include <range/v3/view/subrange.hpp>
#include <unifex/tag_invoke.hpp>

#include <algo/prelude.hpp>

namespace algo
{

namespace _quick_sort
{

template <class Relation, class Projection, class Partition>
struct Algorithm
{
    [[no_unique_address]] Relation relation;
    [[no_unique_address]] Projection projection;
    [[no_unique_address]] Partition partition_algorithm;

    constexpr void operator()(auto first, auto last) const
    {
        auto size = ranges::distance(first, last);
        if (size < 2) {
            return;
        }
        auto const pivot = *(first + ((size + 1) / 2));

        auto middle1 = partition_algorithm(first, last, [&, this](auto const& elm) {
            return relation(projection(elm), projection(pivot));
        });

        auto middle2 = partition_algorithm(middle1, last, [&, this](auto const& elm) {
            return !relation(projection(pivot), projection(elm));
        });

        // all equal elems
        if (middle1 != last) {
            (*this)(first, middle1);
        }

        if (middle2 != first) {
            (*this)(middle2, last);
        }
    }
};

template <class Range, class Relation, class Projection, class Partition>
requires ranges::range<Range>
constexpr void algorithm(Range&& range,
                         Relation&& relation,
                         Projection&& projection,
                         Partition&& partition)
{
    Algorithm const alg{std::forward<Relation>(relation),
                        std::forward<Projection>(projection),
                        std::forward<Partition>(partition)};
    alg(ranges::begin(range), ranges::end(range));
}

template <class Itr, class Sentinel, class Relation, class Projection, class Partition>
constexpr void algorithm(Itr&& first,
                         Sentinel&& last,
                         Relation&& relation,
                         Projection&& projection,
                         Partition&& partition)
{
    Algorithm const alg{std::forward<Relation>(relation),
                        std::forward<Projection>(projection),
                        std::forward<Partition>(partition)};
    alg(std::forward<Itr>(first), std::forward<Sentinel>(last));
}

template <class Relation, class Projection, class Paritition>
struct _adapter final
{
    struct type;
};

template <class Relation, class Projection, class Partition>
using adapter = _adapter<std::decay_t<Relation>,
                         std::decay_t<Projection>,
                         std::decay_t<Partition>>::type;

} // namespace _quick_sort

inline constexpr struct quick_sort_fn
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity,
              class Partition = unifex::tag_t<hoare_partition>>
    requires(not(ranges::range<Relation> or ranges::range<Projection> or
                 ranges::range<Partition>))
    static constexpr auto operator()(Relation&& relation = {},
                                     Projection&& projection = {},
                                     Partition&& partition = {})
        -> _quick_sort::adapter<Relation, Projection, Partition>
    {
        return {std::forward<Relation>(relation),
                std::forward<Projection>(projection),
                std::forward<Partition>(partition)};
    }

    template <class Range, class Relation, class Projection, class Partition>
    requires unifex::tag_invocable<quick_sort_fn, Range, Relation, Projection, Partition>
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation,
                                     Projection&& projection,
                                     Partition&& partition)
        noexcept(unifex::is_nothrow_tag_invocable_v<quick_sort_fn,
                                                    Range,
                                                    Relation,
                                                    Projection,
                                                    Partition>)
            -> unifex::
                tag_invoke_result_t<quick_sort_fn, Range, Relation, Projection, Partition>
    {
        return tag_invoke(quick_sort_fn{},
                          std::forward<Range>(range),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection),
                          std::forward<Partition>(partition));
    }

    template <class Itr,
              class Sentinel,
              class Relation,
              class Projection,
              class Partition>
    requires unifex::tag_invocable<quick_sort_fn,
                                   Itr,
                                   Sentinel,
                                   Relation,
                                   Projection,
                                   Partition>
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation,
                                     Projection&& projection,
                                     Partition&& partition)
        noexcept(unifex::is_nothrow_tag_invocable_v<quick_sort_fn,
                                                    Itr,
                                                    Sentinel,
                                                    Relation,
                                                    Projection,
                                                    Partition>)
            -> unifex::tag_invoke_result_t<quick_sort_fn,
                                           Itr,
                                           Sentinel,
                                           Relation,
                                           Projection,
                                           Partition>
    {
        return tag_invoke(quick_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection),
                          std::forward<Partition>(partition));
    }

    /**
     * @brief Calls the default quick sort implementation for generic random access ranges
     *
     * @tparam Range A type that satisfies sized_range and random_access_range concepts
     * @tparam Relation A callable that satisfies strict_weak_order
     * @tparam Projection A callable that projects into the value types held by `Range`
     * @tparam Partition A partition scheme, defaults to hoare_partition
     * @param range A universal reference of type `Range`
     * @param relation A universal reference of type `Relation`
     * @param projection A universal reference of type `Projection`
     * @param partition A universal reference of type `Partition`
     * @return `sorted_view<Range, Relation, Projection>`
     */
    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity,
              class Partition = unifex::tag_t<hoare_partition>>
    requires(not unifex::
                 tag_invocable<quick_sort_fn, Range, Relation, Projection, Partition> and
             ranges::random_access_range<Range> and ranges::sized_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {},
                                     Partition&& partition = {})
        noexcept(noexcept(_quick_sort::algorithm(begin(range),
                                                 end(range),
                                                 relation,
                                                 projection,
                                                 std::forward<Partition>(partition))))
            -> sorted<Range, Relation, Projection>
    {
        _quick_sort::algorithm(ranges::begin(range),
                               ranges::end(range),
                               relation,
                               projection,
                               std::forward<Partition>(partition));
        return {std::forward<Range>(range),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }

    /**
     * @brief Calls the default quick sort implementation for generic random access ranges
     *
     * @tparam Itr A type that satisfies `ranges::random_access_iterator` concept
     * @tparam Sentinel A type that satisfies `ranges::sentinel_for<Sentinel, Itr>`
     * @tparam Relation A callable that satisfies strict_weak_order
     * @tparam Projection A callable that projects into the value types reference by `Itr`
     * @tparam Partition A partition scheme, defaults to hoare_partition
     * @param first A universal reference of type `Itr`
     * @param last A universal reference of type `Sentinel`
     * @param relation A universal reference of type `Relation`
     * @param projection A universal reference of type `Projection`
     * @param partition A universal reference of type `Partition`
     * @return `sorted_view<Range, Relation, Projection>`
     */
    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity,
              class Partition = unifex::tag_t<hoare_partition>>
    requires(not unifex::tag_invocable<quick_sort_fn,
                                       Itr,
                                       Sentinel,
                                       Relation,
                                       Projection,
                                       Partition> and
             ranges::random_access_iterator<std::remove_cvref_t<Itr>> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Itr>> and
             ranges::indirect_strict_weak_order<Relation,
                                                ranges::projected<Itr, Projection>,
                                                ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {},
                                     Partition&& partition = {})
        noexcept(noexcept(_quick_sort::algorithm(
            first, last, relation, projection, std::forward<Partition>(partition))))
            -> sorted<Itr, Sentinel, Relation, Projection>
    {
        _quick_sort::algorithm(
            first, last, relation, projection, std::forward<Partition>(partition));
        return {ranges::subrange{std::forward<Itr>(first), std::forward<Sentinel>(last)},
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }
} quick_sort;

namespace _quick_sort
{
template <class Relation, class Projection, class Partition>
struct _adapter<Relation, Projection, Partition>::type final
{
    [[no_unique_address]] Relation relation;
    [[no_unique_address]] Projection projection;
    [[no_unique_address]] Partition partition_algorithm; // NOLINT

    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type> and ranges::range<Range>)
    friend constexpr auto operator|(Range&& range, Adapter&& adapter)
    {
        return quick_sort(std::forward<Range>(range),
                          std::forward_like<Adapter>(adapter.relation),
                          std::forward_like<Adapter>(adapter.projection),
                          std::forward_like<Adapter>(adapter.partition_algorithm));
    }
};
} // namespace _quick_sort
} // namespace algo

#include <algo/prologue.hpp>
