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

#include <range/v3/range_concepts.hpp>

#include "ordering.hpp"

namespace algo
{

namespace _insertion_sort
{

template <class Ordering, class T = void>
struct pred;

template <template <class> class Cmp, class T>
struct pred<Cmp<T>, T>
{
    using type = Cmp<T>;
};

template <template <class> class Cmp, class T>
struct pred<Cmp<void>, T>
{
    using type = Cmp<T>;
};

template <class T>
struct pred<ordering::ascending, T>
{
    using type = std::less<T>;
};

template <class T>
struct pred<ordering::descending, T>
{
    using type = std::greater<T>;
};

template <class Ordering, class T>
using pred_t = typename pred<Ordering, T>::type;

template <class T>
decltype(auto) move_or_copy(T&& val)
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
template <std::ranges::forward_range RNG, class CMP = std::less<>>
requires(std::is_nothrow_swappable_v<std::ranges::range_value_t<RNG>>)
auto insertion_sort(RNG&& rng, CMP const& /*compare tag*/ = {})
{
    pred_t<CMP, ranges::range_value_t<decltype(rng)>> const compare{};

    using std::swap;
    if (rng.begin() == rng.end()) {
        return rng;
    }

    // assume range of size 1 on left is sorted
    auto key_itr = rng.begin();
    ++key_itr;

    // TODO: Document loop invariants.
    for (; key_itr != rng.end(); ++key_itr) {
        // use first element of right range as key to sort into left range
        auto key = move_or_copy(*key_itr);

        // find position of key in left range
        auto i = rng.begin();
        while (!compare(key, *i) and i != key_itr) {
            ++i;
        }

        // shift elements from i to key_iter by one
        // TODO: if RNG is contiguous and value_type is trivially
        // relocatable then use memcpy
        if (i != key_itr) {
            std::ranges::range_value_t<RNG> tmp{move_or_copy(*i)};

            auto j = i;
            ++j;
            do { // NOLINT
                swap(tmp, *j);
                if (j == key_itr) {
                    break;
                }
                else {
                    ++j;
                }
            } while (true);
        }

        // insert key into i
        swap(key, *i);
    }
    return rng;
}

template <class COMPARE, class SCHED = void>
struct insertion_sort_adapter;

template <class Compare>
struct insertion_sort_adapter<Compare, void>
{
    Compare ord;

    friend constexpr auto operator|(
        ranges::forward_range auto range,
        [[maybe_unused]] insertion_sort_adapter const& adapter)
    {
        return insertion_sort(std::move(range), adapter.ord);
    }
};

template <class COMPARE, class SCHED>
struct insertion_sort_adapter
{
    // Scheduler
    SCHED sched;

    friend constexpr auto operator|(
        ranges::forward_range auto range,
        [[maybe_unused]] insertion_sort_adapter&& adapter)
    {
        return insertion_sort(
            std::move(range),
            std::forward<decltype(adapter)>(adapter).sched,
            pred_t<COMPARE, ranges::range_value_t<decltype(range)>>{});
    }
};
} // namespace _insertion_sort

constexpr struct
{
    template <class Ordering = ordering::ascending>
    static constexpr auto operator()(
        [[maybe_unused]] Ordering const& ordering = {})
    {
        return _insertion_sort::insertion_sort_adapter<Ordering>{ordering};
    }

    template <class Ordering = ordering::ascending>
    static constexpr auto operator()(ranges::forward_range auto range,
                                     Ordering const& ordering = Ordering{})
    {
        return _insertion_sort::insertion_sort(std::move(range), ordering);
    }

    static constexpr auto operator()()
    {
        return _insertion_sort::insertion_sort_adapter<
            ordering::ascending>{};
    }

    _insertion_sort::pred_t<ordering::ascending, void> ascending{};
    _insertion_sort::pred_t<ordering::descending, void> decending{};

} insertion_sort;

constexpr auto insertion_sort_decending =
    insertion_sort(ordering::descending{});
constexpr auto insertion_sort_ascending =
    insertion_sort(ordering::ascending{});

} // namespace algo
