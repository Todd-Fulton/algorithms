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

#include <range/v3/algorithm.hpp>
#include <range/v3/all.hpp>
#include <range/v3/iterator/traits.hpp>

#include "insertion_sort.hpp"

namespace algo
{

namespace _tim_sort
{
// Merge function merges the sorted runs
void merge(auto& cache,
           auto left_start,
           auto left_end,
           auto right_end,
           auto&& relation,
           auto&& projection)
{
    cache.insert(cache.end(),
                 std::make_move_iterator(left_start),
                 std::make_move_iterator(right_end));

    auto i = cache.begin();
    auto j = i + ranges::distance(left_start, left_end);
    auto const i_end = j;
    auto const j_end = cache.end();

    auto insert_itr = left_start;

    // After comparing, we
    // merge those two array
    // in larger sub array
    while (i < i_end && j < j_end) {
        if (relation(projection(*i), projection(*j))) {
            *insert_itr = ranges::iter_move(i);
            ++i;
        }
        else {
            *insert_itr = ranges::iter_move(j);
            ++j;
        }
        ++insert_itr;
    }

    // Copy remaining elements of
    // left, if any
    while (i < i_end) {
        *insert_itr = ranges::iter_move(i);
        ++insert_itr;
        ++i;
    }

    // Copy remaining element of
    // right, if any
    while (j < j_end) {
        *insert_itr = ranges::iter_move(j);
        ++insert_itr;
        ++j;
    }
}

template <ranges::contiguous_iterator Itr, class Relation, class Projection>
void algorithm(Itr first, Itr last, Relation&& relation, Projection&& projection)
{
    using diff_t = ranges::iter_difference_t<std::remove_cvref_t<Itr>>;

    static constexpr diff_t RUN = 32;

    auto range = ranges::subrange(first, last);
    auto const size = ranges::size(range);

    // Sort individual subranges of size RUN
    ranges::for_each(range | ranges::views::chunk(RUN),
                     [&](auto&& chunk) { insertion_sort(chunk, relation, projection); });

    std::vector<ranges::iter_value_t<std::remove_cvref_t<Itr>>> cache;
    cache.reserve(size_t(size));

    // Start merging from size RUN * 2^0, RUN * 2^1, RUN * 2^2, ...
    for (diff_t chunk_size = RUN; chunk_size < diff_t(size); chunk_size *= 2) {
        ranges::for_each(range | ranges::views::chunk(chunk_size * 2), [&](auto&& chunk) {
            auto chunk_first = ranges::begin(chunk).base();
            auto chunk_mid = chunk_first + chunk_size;
            auto chunk_last = std::min(chunk_mid + chunk_size, last.base());
            if (chunk_mid < chunk_last) {
                merge(cache, chunk_first, chunk_mid, chunk_last, relation, projection);
                cache.clear();
            }
        });
    }
}

template <class Relation, class Projection>
struct _adapter final
{
    struct type;
};

template <class Relation, class Projection>
using adapter = _adapter<std::decay_t<Relation>, std::decay_t<Projection>>::type;

} // namespace _tim_sort

inline constexpr struct tim_sort_fn
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(not(ranges::range<Relation> or ranges::range<Projection>))
    static constexpr auto operator()(Relation&& relation = {},
                                     Projection&& projection = {})
        -> _tim_sort::adapter<Relation, Projection>
    {
        return {std::forward<Relation>(relation), std::forward<Projection>(projection)};
    }

    template <class Range,
              class Relation = ordering::ascending_fn,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<tim_sort_fn, Range, Relation, Projection>
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation,
                                     Projection&& projection)
        noexcept(
            unifex::is_nothrow_tag_invocable_v<tim_sort_fn, Range, Relation, Projection>)
            -> unifex::tag_invoke_result_t<tim_sort_fn, Range, Relation, Projection>
    {
        return tag_invoke(tim_sort_fn{},
                          std::forward<Range>(range),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Itr, class Sentinel, class Relation, class Projection>
    requires unifex::tag_invocable<tim_sort_fn, Itr, Sentinel, Relation, Projection>
    static constexpr auto operator()(
        Itr&& first,
        Sentinel&& last,
        Relation&& relation,
        Projection&& projection) noexcept(unifex::is_nothrow_tag_invocable_v<tim_sort_fn,
                                                                             Itr,
                                                                             Sentinel,
                                                                             Relation,
                                                                             Projection>)
        -> unifex::tag_invoke_result_t<tim_sort_fn, Itr, Sentinel, Relation, Projection>
    {
        return tag_invoke(tim_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
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
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<tim_sort_fn, Range, Relation, Projection> and
             ranges::random_access_range<Range> and ranges::sized_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(
            _tim_sort::algorithm(begin(range), end(range), relation, projection)))
            -> sorted<Range, Relation, Projection>
    {
        _tim_sort::algorithm(
            ranges::begin(range), ranges::end(range), relation, projection);
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
              class Projection = ranges::identity>
    requires(not unifex::
                 tag_invocable<tim_sort_fn, Itr, Sentinel, Relation, Projection> and
             ranges::random_access_iterator<std::remove_cvref_t<Itr>> and
             ranges::sentinel_for<std::remove_cvref_t<Sentinel>,
                                  std::remove_cvref_t<Itr>> and
             ranges::indirect_strict_weak_order<Relation,
                                                ranges::projected<Itr, Projection>,
                                                ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(_tim_sort::algorithm(first, last, relation, projection)))
            -> sorted<Itr, Sentinel, Relation, Projection>
    {
        _tim_sort::algorithm(first, last, relation, projection);
        return {std::forward<Itr>(first),
                std::forward<Sentinel>(last),
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }
} tim_sort;

namespace _tim_sort
{
template <class Relation, class Projection>
struct _adapter<Relation, Projection>::type final
{
    [[no_unique_address]] Relation relation;
    [[no_unique_address]] Projection projection;

    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type> and ranges::range<Range>)
    friend constexpr auto operator|(Range&& range, Adapter&& adapter)
    {
        return tim_sort(std::forward<Range>(range),
                        std::forward_like<Adapter>(adapter.relation),
                        std::forward_like<Adapter>(adapter.projection));
    }
};
} // namespace _tim_sort
} // namespace algo
