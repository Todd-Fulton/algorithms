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

#include <range/v3/range/concepts.hpp>
#include <unifex/tag_invoke.hpp>

#include <cassert>

#include <algo/prelude.hpp>

namespace algo
{
namespace _alexandrescu_partition
{

/**
 * @brief Alexandrescu partition scheme using sentinals
 *
 * Preconditions:
 *   - Types held by the container must be in a valid
 *   state after a move operation and the predicate/projection
 *   combination must be able to handle such a case and do the
 *   appropriate thing. Typically, a moved from value should be
 *   less than all other values.
 *
 * @tparam Predicate [TODO:tparam]
 * @tparam Projection [TODO:tparam]
 * @param range [TODO:parameter]
 * @param predicate [TODO:parameter]
 * @param projection [TODO:parameter]
 * @return [TODO:return]
 */
template <class Iter, class Predicate, class Projection>
requires(std::bidirectional_iterator<Iter> and std::bidirectional_iterator<Iter> and
         ranges::indirect_unary_predicate<Predicate, ranges::projected<Iter, Projection>>)
constexpr auto algorithm(Iter first,
                         Iter last,
                         Predicate&& predicate,
                         Projection&& projection)
    noexcept(std::is_nothrow_move_assignable_v<std::iter_value_t<Iter>> and
             std::is_nothrow_invocable_v<Projection, std::iter_value_t<Iter>> and
             std::is_nothrow_invocable_v<
                 Predicate,
                 std::invoke_result_t<Projection, std::iter_value_t<Iter>>>) -> Iter
{

    // Find start positions
    while (first != last and predicate(projection(*first))) {
        ++first;
    }
    while (last != first and !predicate(projection(*(--last)))) {}

    if (ranges::distance(first, last) > 0) {
        // ensure while loops terminate with predicate
        ranges::iter_swap(first, last);

        // copy
        auto original_succ = *last;

        // now we can loop without checking bounds
        while (true) {

            // find next successor value
            // NOTE: this may evaluate a moved-from value located at
            // `successor` after the first iteration
            do { // NOLINT *do*
                ++first;
            } while (predicate(projection(*first)));

            // NOTE: if predecessor >= successor, we have evaluated a moved from
            // value

            *last = ranges::iter_move(first);
            // hole is now at predecessor

            // find next predecessor value
            // NOTE: this may evaluate a moved-from value located at
            // `predecessor`
            do { // NOLINT *do*
                --last;
            } while (!predicate(projection(*last)));

            // NOTE: we may have moved passed the moved from value
            // if a moved-from value is evaluated as a predecessor.
            // To fix this, use copies instead of moves.
            if (ranges::distance(first, last) <= 0) {
                break;
            }

            *first = ranges::iter_move(last);
            // hole is now at successor
        }

        // Handle case where range is already partitioned
        if (first == last + 2) {
            ++last;
            if (predicate(projection(*last))) {
                *first = ranges::iter_move(last);
                --first;
            }
        }

        *first = std::move(original_succ);
    }
    return first;
}

template <class Predicate, class Projection>
struct _adapter
{
    struct type;
};

template <class Predicate, class Projection>
using adapter = _adapter<Predicate, Projection>::type;

namespace _cpo
{

} // namespace _cpo
} // namespace _alexandrescu_partition

inline constexpr struct alexandrescu_partition_fn
{
    template <class Range, class Predicate, class Projection = ranges::identity>
    requires(unifex::tag_invocable<alexandrescu_partition_fn,
                                   Range,
                                   Predicate,
                                   Projection> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    constexpr auto operator()(Range&& range,
                              Predicate&& predicate,
                              Projection&& projection = {}) const
        noexcept(unifex::is_nothrow_tag_invocable_v<alexandrescu_partition_fn,
                                                    Range,
                                                    Predicate,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<alexandrescu_partition_fn,
                                           Range,
                                           Predicate,
                                           Projection>
    {
        return tag_invoke(*this, FWD(range), FWD(predicate), FWD(projection));
    }

    template <class Iter, class Predicate, class Projection = ranges::identity>
    requires(unifex::tag_invocable<alexandrescu_partition_fn,
                                   Iter,
                                   Iter,
                                   Predicate,
                                   Projection> and
             ranges::indirect_unary_predicate<Predicate,
                                              ranges::projected<Iter, Projection>>)
    constexpr auto operator()(Iter&& first,
                              Iter&& last,
                              Predicate&& predicate,
                              Projection&& projection = {}) const
        noexcept(unifex::is_nothrow_tag_invocable_v<alexandrescu_partition_fn,
                                                    Iter,
                                                    Iter,
                                                    Predicate,
                                                    Projection>)
            -> unifex::tag_invoke_result_t<alexandrescu_partition_fn,
                                           Iter,
                                           Iter,
                                           Predicate,
                                           Projection>
    {
        return tag_invoke(*this, FWD(first), FWD(last), FWD(predicate), FWD(projection));
    }

    template <class Predicate, class Projection = ranges::identity>
    requires(!ranges::range<Predicate>)
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {}) //
        noexcept(std::is_nothrow_constructible_v<
                 _alexandrescu_partition::adapter<Predicate, Projection>,
                 Predicate,
                 Projection>) -> _alexandrescu_partition::adapter<Predicate, Projection>
    {
        return {std::forward<Predicate>(predicate), std::forward<Projection>(projection)};
    }

    template <class Range, class Predicate, class Projection = ranges::identity>
    requires(not unifex::tag_invocable<alexandrescu_partition_fn,
                                       Range,
                                       Predicate,
                                       Projection> and
             ranges::bidirectional_range<Range> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    static constexpr auto operator()(Range&& range,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(noexcept(_alexandrescu_partition::algorithm(
            begin(range), end(range), FWD(predicate), FWD(projection))))
            -> std::pair<ranges::iterator_t<Range>, std::remove_cvref_t<Range>>
    {
        return std::pair(_alexandrescu_partition::algorithm(
                             begin(range), end(range), FWD(predicate), FWD(projection)),
                         FWD(range));
    }

    template <class Iter, class Predicate, class Projection = ranges::identity>
    requires(not unifex::tag_invocable<alexandrescu_partition_fn,
                                       Iter,
                                       Iter,
                                       Predicate,
                                       Projection> and
             ranges::bidirectional_iterator<Iter> and
             ranges::indirect_unary_predicate<Predicate,
                                              ranges::projected<Iter, Projection>>)
    static constexpr auto operator()(Iter&& first,
                                     Iter last,
                                     Predicate&& predicate,
                                     Projection&& projection = {})
        noexcept(noexcept(_alexandrescu_partition::algorithm(
            FWD(first), FWD(last), FWD(predicate), FWD(projection)))) -> Iter
    {
        return _alexandrescu_partition::algorithm(
            FWD(first), FWD(last), FWD(predicate), FWD(projection));
    }
} alexandrescu_partition;

namespace _alexandrescu_partition
{

template <class Predicate, class Projection>
struct _adapter<Predicate, Projection>::type
{
    Predicate pred_;
    Projection proj_;

private:
    template <class Adapter, class Range>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>)
    constexpr friend auto operator|(Range&& range, Adapter&& self)
        noexcept(noexcept(alexandrescu_partition(std::forward<Range>(range),
                                                 std::forward<Predicate>(self.pred_),
                                                 std::forward<Projection>(self.proj_))))
            -> decltype(alexandrescu_partition(std::forward<Range>(range),
                                               std::forward<Predicate>(self.pred_),
                                               std::forward<Projection>(self.proj_)))
    {
        return alexandrescu_partition(std::forward<Range>(range),
                                      std::forward<Predicate>(self.pred_),
                                      std::forward<Projection>(self.proj_));
    }
};
} // namespace _alexandrescu_partition
} // namespace algo
