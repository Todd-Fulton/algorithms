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

#include <algo/sort/ordering.hpp>
#include <algo/sort/sorted.hpp>

#include <range/v3/algorithm.hpp>
#include <unifex/tag_invoke.hpp>

namespace algo
{

inline constexpr struct is_sorted_fn
{
    template <class Range,
              class Relation = unifex::tag_t<algo::ordering::ascending>,
              class Projection = ranges::identity>
    requires(not Sorted<Range> and
             not unifex::tag_invocable<is_sorted_fn, Range, Relation, Projection> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr bool operator()(Range&& range,
                                     Relation&& relation = {},
                                     Projection&& projection = {})
    {
        return ranges::is_sorted(std::forward<Range>(range),
                                 std::forward<Relation>(relation),
                                 std::forward<Projection>(projection));
    }

    template <class Range,
              class Relation = unifex::tag_t<algo::ordering::ascending>,
              class Projection = ranges::identity>
    requires(Sorted<Range> and
             not unifex::tag_invocable<is_sorted_fn, Range, Relation, Projection> and
             ranges::indirect_strict_weak_order<
                 Relation,
                 ranges::projected<ranges::iterator_t<Range>, Projection>> and
             std::same_as<typename std::remove_cvref_t<Range>::relation_t,
                          std::remove_cvref_t<Relation>> and
             std::same_as<typename std::remove_cvref_t<Range>::projection_t,
                          std::remove_cvref_t<Projection>>)
    constexpr bool is_sorted([[maybe_unused]] Range&& range,
                             [[maybe_unused]] Relation&& relation = {},
                             [[maybe_unused]] Projection&& projection = {})
    {
        return true;
    }

    template <Sorted Range>
    requires(not unifex::tag_invocable<is_sorted_fn, Range>)
    static constexpr bool operator()([[maybe_unused]] Range&& /*range*/)
    {
        return true;
    }

    template <class Range, class... Args>
    requires unifex::tag_invocable<is_sorted_fn, Range, Args...>
    static constexpr auto operator()(Range&& range, Args&&... args)
        noexcept(unifex::is_nothrow_tag_invocable_v<is_sorted_fn, Range, Args...>)
            -> unifex::tag_invoke_result_t<is_sorted_fn, Range, Args...>
    {
        return unifex::tag_invoke(
            is_sorted_fn{}, std::forward<Range>(range), std::forward<Args>(args)...);
    }
} is_sorted;

} // namespace algo
