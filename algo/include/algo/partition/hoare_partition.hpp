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

#define ITR_VALUE_T(itr) std::iter_value_t<decltype(itr)>
#define REMOVE_CVREF_T(var) std::remove_cvref_t<decltype(var)>

namespace algo
{
namespace _hoare_partition
{
template <std::bidirectional_iterator Iter1, std::bidirectional_iterator Iter2>
constexpr auto
algorithm(Iter1 first, Iter2 last, auto&& predicate, auto&& projection) noexcept(
    std::is_nothrow_move_assignable_v<ITR_VALUE_T(first)> and
    std::is_nothrow_invocable_v<decltype(projection), ITR_VALUE_T(first)> and
    std::is_nothrow_invocable_v<
        decltype(predicate),
        std::invoke_result_t<decltype(projection), ITR_VALUE_T(first)>>) -> Iter1
{
    Iter1 ret = last;
    if (ranges::distance(first, last) < 2) {
        ret = last;
    }
    else {
        auto low = first;
        auto high = prev(last);

        while (true) {
            while (predicate(projection(*low)) and low < high) {
                ++low;
            }
            while (!predicate(projection(*high)) and low < high) {
                --high;
            }
            if (low < high) {
                ranges::iter_swap(low, high);
                ++low;
                --high;
            }
            else {
                if (predicate(projection(*low))) {
                    ++low;
                }
                if (low < last) {
                    ranges::iter_swap(low, prev(last));
                }
                ret = low;
                break;
            }
        }
    }
    return ret;
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
    /**
     * @brief Partition `range` according to predicate.
     *
     * This function takes a range by value. If you need
     * to permute a range by reference, use a view instead,
     * for example `ranges::all` or `ranges::subrange`.
     *
     * Computes the partition around predicate(x) -> true
     * using Hoare's partition scheme.
     *
     * Ex: [0, 2, 10, 2, 5, 9, 6, 8, 7, 3]
     *  is permuted to [0, 2, 3, 2, 5 * 6, 8, 7, 10, 9]
     *  around the predicate (x < 6)
     *
     *  A partition function may be supplied to, for example,
     *  project into a tuple or a structure.
     *
     * Ex: snd(x : tuple<int, float>) -> float
     *
     * @tparam Range A container type satisfying the bidirectional_range concept
     * @tparam Predicate A unary function of type T -> bool
     * @tparam Projection A unary function of type T -> U
     *
     * @param range A range of type `Range`
     * @param predicate The function to discriminate between predecessor and
     * successor elements in `range`
     * @param projection A function to "project into" elements of `range`
     * defaults to the identity function id(x) -> x
     *
     * @return a pair (iterator, range) where the iterator points to the
     * element where successor elements begin.
     */
    template <ranges::bidirectional_range Range,
              class Predicate,
              class Projection = ranges::identity>
    requires ranges::indirect_unary_predicate<
        Predicate,
        ranges::projected<ranges::iterator_t<Range>, Projection>>
    constexpr auto operator()(Range&& range,
                              Predicate&& predicate,
                              Projection&& projection = {}) const
    {
        return tag_invoke(*this, FWD(range), FWD(predicate), FWD(projection));
    }

    /**
     * @brief Partition a range of elements according to predicate.
     *
     * Computes the partition around predicate(x) -> true
     * using Hoare's partition scheme.
     *
     * Ex: [0, 2, 10, 2, 5, 9, 6, 8, 7, 3]
     *  is permuted to [0, 2, 3, 2, 5 * 6, 8, 7, 10, 9]
     *  around the predicate (x < 6)
     *
     *  A partition function may be supplied to, for example,
     *  project into a tuple or a structure.
     *
     * Ex: snd(x : tuple<int, float>) -> float
     *
     * @tparam Iter1 An iterator type satisfying the bidirectional_range concept
     * @tparam Iter2 An iterator type satisfying the bidirectional_range concept
     * @tparam Predicate A unary function of type T -> bool
     * @tparam Projection A unary function of type T -> U
     *
     * @param first An iterator pointing to the first element in the range.
     * @param last An iterator pointing to the last element in the range.
     * @param predicate The function to discriminate between predecessor and
     * successor elements in `range`
     * @param projection A function to "project into" elements of `range`
     * defaults to the identity function id(x) -> x
     *
     * @return an iterator where the iterator points to the
     * element where successor elements begin.
     */
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
    {
        return tag_invoke(
            *this, FWD(first), FWD(last), FWD(predicate), FWD(projection));
    }
    template <class Predicate, class Projection = ranges::identity>
    requires(!ranges::range<Predicate>)
    static constexpr auto operator()(Predicate&& predicate,
                                     Projection&& projection = {})
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
            ranges::identity{}) noexcept(noexcept(std::pair(algorithm(begin(range),
                                                                   end(range),
                                                                   FWD(predicate),
                                                                   FWD(projection)),
                                                         FWD(range))))
        -> std::pair<RNG_ITR_T(range), std::remove_cvref_t<Range>>
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
        Iter2&& last,
        Predicate&& predicate,
        Projection&& projection =
            ranges::identity{}) noexcept(noexcept(algorithm(FWD(first),
                                                         FWD(last),
                                                         FWD(predicate),
                                                         FWD(projection)))) -> Iter1
    {
        return algorithm(FWD(first), FWD(last), FWD(predicate), FWD(projection));
    }
};
} // namespace _cpo
} // namespace _hoare_partition

/**
 * Partition a range of elements according to predicate.
 *
 * Computes the partition around predicate(x) -> true
 * using Hoare's partition scheme.
 *
 * Ex: [0, 2, 10, 2, 5, 9, 6, 8, 7, 3]
 *  is permuted to [0, 2, 3, 2, 5 * 6, 8, 7, 10, 9]
 *  around the predicate (x < 6)
 *
 *  A partition function may be supplied to, for example,
 *  project into a tuple or a structure.
 *
 * Ex: snd(x : tuple<int, float>) -> float
 */
constexpr auto hoare_partition = _hoare_partition::_cpo::_fn{};

namespace _hoare_partition
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
        unifex::is_nothrow_tag_invocable_v<unifex::tag_t<hoare_partition>,
                                           Range,
                                           decltype(FWD(self).pred_),
                                           decltype(FWD(self).proj_)>)
        -> unifex::tag_invoke_result_t<unifex::tag_t<hoare_partition>,
                                       Range,
                                       decltype(FWD(self).pred_),
                                       decltype(FWD(self).proj_)>
    {
        return tag_invoke(
            hoare_partition, FWD(range), FWD(self).pred_, FWD(self).proj_);
    }
};
} // namespace _hoare_partition


} // namespace algo

#include <algo/prologue.hpp>
