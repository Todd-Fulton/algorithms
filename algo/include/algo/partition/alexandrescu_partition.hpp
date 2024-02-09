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
template <std::bidirectional_iterator Iter1,
          std::bidirectional_iterator Iter2,
          class Predicate,
          class Projection>
constexpr auto algorithm(
    Iter1 first,
    Iter2 last,
    Predicate&& predicate,
    Projection&&
        projection) noexcept(std::
                                 is_nothrow_move_assignable_v<
                                     std::iter_value_t<Iter1>> and
                             std::is_nothrow_invocable_v<
                                 Projection,
                                 std::iter_value_t<Iter1>> and
                             std::is_nothrow_invocable_v<
                                 Predicate,
                                 std::invoke_result_t<Projection,
                                                      std::iter_value_t<Iter1>>>)
    -> Iter1
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
using adapter = _adapter<std::decay_t<Predicate>, std::decay_t<Projection>>::type;

namespace _cpo
{

struct _fn
{
    template <ranges::bidirectional_range Range,
              class Predicate,
              class Projection = ranges::identity>
    requires ranges::indirect_unary_predicate<
        Predicate,
        ranges::projected<ranges::iterator_t<Range>, Projection>>
    constexpr auto operator()(Range&& range,
                              Predicate&& predicate,
                              Projection&& projection = {}) const
        noexcept(
            unifex::is_nothrow_tag_invocable_v<_fn, Range, Predicate, Projection>)
            -> unifex::tag_invoke_result_t<_fn, Range, Predicate, Projection>
    {
        return tag_invoke(*this, FWD(range), FWD(predicate), FWD(projection));
    }

    template <std::bidirectional_iterator Iter1,
              std::bidirectional_iterator Iter2,
              class Predicate,
              class Projection = ranges::identity>
    requires ranges::indirect_unary_predicate<Predicate,
                                              ranges::projected<Iter1, Projection>>
    constexpr auto operator()(Iter1&& first,
                              Iter2&& last,
                              Predicate&& predicate,
                              Projection&& projection = {}) const
        noexcept(
            unifex::
                is_nothrow_tag_invocable_v<_fn, Iter1, Iter2, Predicate, Projection>)
            -> unifex::tag_invoke_result_t<_fn, Iter1, Iter2, Predicate, Projection>
    {
        return tag_invoke(
            *this, FWD(first), FWD(last), FWD(predicate), FWD(projection));
    }

    template <class Predicate, class Projection = ranges::identity>
    requires(!ranges::range<Predicate>)
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {}) //
        noexcept(std::is_nothrow_constructible_v<adapter<Predicate, Projection>,
                                                 Predicate,
                                                 Projection>)
            -> adapter<Predicate, Projection>
    {
        return adapter<Predicate, Projection>{FWD(predicate), FWD(projection)};
    }

private:
    template <ranges::bidirectional_range Range,
              class Predicate,
              class Projection = ranges::identity>
    requires ranges::indirect_unary_predicate<
        Predicate,
        ranges::projected<ranges::iterator_t<Range>, Projection>>
    friend constexpr auto tag_invoke(
        _fn const& /*f*/,
        Range&& range,
        Predicate&& predicate,
        Projection&& projection =
            ranges::identity{}) noexcept(noexcept(algorithm(begin(range),
                                                            end(range),
                                                            FWD(predicate),
                                                            FWD(projection))))
        -> std::pair<ranges::iterator_t<Range>, std::remove_cvref_t<Range>>
    {
        return std::pair(
            algorithm(begin(range), end(range), FWD(predicate), FWD(projection)),
            FWD(range));
    }

    template <std::bidirectional_iterator Iter1,
              std::bidirectional_iterator Iter2,
              class Predicate,
              class Projection = ranges::identity>
    requires ranges::indirect_unary_predicate<Predicate,
                                              ranges::projected<Iter1, Projection>>
    friend constexpr auto tag_invoke(
        _fn const& /*f*/,
        Iter1&& first,
        Iter2 last,
        Predicate&& predicate,
        Projection&& projection =
            ranges::identity{}) noexcept(noexcept(algorithm(FWD(first),
                                                            FWD(last),
                                                            FWD(predicate),
                                                            FWD(projection))))
        -> Iter1
    {
        return algorithm(FWD(first), FWD(last), FWD(predicate), FWD(projection));
    }
};
} // namespace _cpo
} // namespace _alexandrescu_partition

constexpr auto alexandrescu_partition = _alexandrescu_partition::_cpo::_fn{};

namespace _alexandrescu_partition
{

template <class Predicate, class Projection>
struct _adapter<Predicate, Projection>::type
{
    Predicate pred_;
    Projection proj_;

    template <class Adapter, ranges::bidirectional_range Range>
    requires std::same_as<std::remove_cvref_t<Adapter>, type> and
             ranges::indirect_unary_predicate<
                 Predicate,
                 ranges::projected<ranges::iterator_t<Range>, Projection>>
    constexpr friend auto operator|(Range&& range, Adapter&& self) noexcept(
        unifex::is_nothrow_tag_invocable_v<unifex::tag_t<alexandrescu_partition>,
                                           Range,
                                           decltype(FWD(self).pred_),
                                           decltype(FWD(self).proj_)>)
        -> unifex::tag_invoke_result_t<unifex::tag_t<alexandrescu_partition>,
                                       Range,
                                       decltype(FWD(self).pred_),
                                       decltype(FWD(self).proj_)>
    {
        return tag_invoke(
            alexandrescu_partition, FWD(range), FWD(self).pred_, FWD(self).proj_);
    }
};
} // namespace _alexandrescu_partition
} // namespace algo
