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

#include <range/v3/algorithm/for_each.hpp>
#include <range/v3/algorithm/move.hpp>
#include <range/v3/iterator/operations.hpp>
#include <unifex/tag_invoke.hpp>
#include <vector>

namespace algo
{

namespace _merge_sort
{

template <class Itr, class Sentinel, class Relation, class Projection>
constexpr void algorithm(Itr&& first,
                         Sentinel&& last,
                         Relation&& relation,
                         Projection&& projection)
{

    // TODO: Convert to loop based algorithm.
    auto const size = ranges::distance(first, last);

    auto merge = [&](auto dest, auto litr, auto lend, auto ritr, auto rend) {
        while (litr != lend and ritr != rend) {
            if (relation(projection(*litr), projection(*ritr))) {
                *dest = ranges::iter_move(litr);
                ++litr;
            }
            else {
                *dest = ranges::iter_move(ritr);
                ++ritr;
            }
            ++dest;
        }

        while (litr != lend) {
            *dest = ranges::iter_move(litr);
            ++dest, ++litr;
        }

        while (ritr != rend) {
            *dest = ranges::iter_move(ritr);
            ++dest, ++ritr;
        }
    };

    auto top_down_split_merge = [&](this auto&& self, auto src, auto dst, auto size) {
        if (size <= 1) {
            return;
        }

        auto sz_left = (size / 2) + (size % 2);
        auto sz_right = size / 2;

        auto sbegin_left{src};
        auto sbegin_right{src + sz_left};

        auto dbegin_left{dst};
        auto dend_left{dst + sz_left};
        auto dbegin_right{dst + sz_left};
        auto dend_right{dst + size};

        self(dbegin_left, sbegin_left, sz_left);
        self(dbegin_right, sbegin_right, sz_right);
        merge(src, dbegin_left, dend_left, dbegin_right, dend_right);
    };

    std::vector<ranges::iter_value_t<Itr>> dst;
    dst.reserve(size_t(size));
    ranges::move(first, last, std::back_inserter(dst));
    auto src_itr = first;
    auto dst_itr = std::begin(dst);

    top_down_split_merge(src_itr, dst_itr, size);
}

template <class Relation, class Projection>
struct _adapter
{
    struct type;
};

template <class Ordering, class Projection>
using adapter = _adapter<std::decay_t<Ordering>, std::decay_t<Projection>>::type;

} // namespace _merge_sort

inline constexpr struct merge_sort_fn
{
    template <class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(!ranges::range<Relation> and !ranges::range<Projection>)
    static constexpr auto operator()(Relation&& ordering = {},
                                     Projection&& projection = {})
        -> _merge_sort::adapter<Relation, Projection>
    {
        return {std::forward<Relation>(ordering), std::forward<Projection>(projection)};
    }

    template <ranges::forward_range Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<merge_sort_fn, Range, Relation, Projection>
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<merge_sort_fn,
                                                    decltype(range),
                                                    Relation,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<merge_sort_fn, Range, Relation, Projection>
    {
        return tag_invoke(merge_sort_fn{},
                          std::forward<Range>(range),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Itr,
              class Sentinel,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires unifex::tag_invocable<merge_sort_fn, Itr, Sentinel, Relation, Projection>
    static constexpr auto operator()(
        Itr&& first,
        Sentinel&& last,
        Relation&& relation = {},
        Projection&& projection =
            {}) noexcept(unifex::is_nothrow_tag_invocable_v<merge_sort_fn,
                                                            Itr,
                                                            Sentinel,
                                                            Relation,
                                                            Projection>)
        -> unifex::tag_invoke_result_t<merge_sort_fn, Itr, Sentinel, Relation, Projection>
    {
        return tag_invoke(merge_sort_fn{},
                          std::forward<Itr>(first),
                          std::forward<Sentinel>(last),
                          std::forward<Relation>(relation),
                          std::forward<Projection>(projection));
    }

    template <class Range,
              class Relation = unifex::tag_t<ordering::ascending>,
              class Projection = ranges::identity>
    requires(not unifex::tag_invocable<merge_sort_fn, Range, Relation, Projection> and
             ranges::forward_range<Range> and ranges::sized_range<Range> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(_merge_sort::algorithm(
            ranges::begin(range), ranges::end(range), relation, projection)))
            -> sorted<Range, Relation, Projection>
    {
        _merge_sort::algorithm(
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
                 tag_invocable<merge_sort_fn, Itr, Sentinel, Relation, Projection> and
             ranges::forward_iterator<Itr> and ranges::sentinel_for<Sentinel, Itr> and
             ranges::indirect_strict_weak_order<Relation,
                                                ranges::projected<Itr, Projection>,
                                                ranges::projected<Itr, Projection>>)
    static constexpr auto operator()(Itr&& first,
                                     Sentinel&& last,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
        noexcept(noexcept(_merge_sort::algorithm(first, last, relation, projection)))
            -> sorted<ranges::subrange<Itr, Sentinel>, Relation, Projection>
    {
        _merge_sort::algorithm(first, last, relation, projection);
        return {ranges::subrange{std::forward<Itr>(first), std::forward<Sentinel>(last)},
                std::forward<Relation>(relation),
                std::forward<Projection>(projection)};
    }
} merge_sort;

namespace _merge_sort
{

template <class Relation, class Projection>
struct _adapter<Relation, Projection>::type
{
    [[no_unique_address]] Relation relation_;     // NOLINT (*reference*)
    [[no_unique_address]] Projection projection_; // NOLINT

    template <class Range, class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(Range&& range, Adapter&& self)
        noexcept(noexcept(merge_sort(std::forward<decltype(range)>(range),
                                     std::forward_like<Adapter>(self.relation_),
                                     std::forward_like<Adapter>(self.projection_))))
            -> decltype(merge_sort(std::forward<decltype(range)>(range),
                                   std::forward_like<Adapter>(self.relation_),
                                   std::forward_like<Adapter>(self.projection_)))
    {
        return merge_sort(std::forward<decltype(range)>(range),
                          std::forward_like<Adapter>(self.relation_),
                          std::forward_like<Adapter>(self.projection_));
    }
};

} // namespace _merge_sort
inline constexpr auto merge_sort_descending = merge_sort(ordering::descending);
inline constexpr auto merge_sort_ascending = merge_sort(ordering::ascending);

} // namespace algo
