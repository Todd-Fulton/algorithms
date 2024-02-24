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

#include <range/v3/iterator/concepts.hpp>
#include <range/v3/range/concepts.hpp>
#include <range/v3/range_concepts.hpp>
#include <type_traits>
#include <unifex/tag_invoke.hpp>

#include "ordering.hpp"

namespace algo
{

namespace _insertion_sort
{

template <class T>
auto&& move_or_copy(T&& val)
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
template <std::forward_iterator Itr,
          ranges::sentinel_for<Itr> Sentinel,
          class CMP = std::less<>>
void algorithm(Itr start, Sentinel end, CMP const& compare = {})
    noexcept(std::is_nothrow_swappable_v<std::iter_value_t<Itr>>)
{
    using std::swap;
    if (start == end) {
        return;
    }

    // assume range of size 1 on left is sorted
    auto key_itr = start;
    ++key_itr;

    // TODO: Document loop invariants.
    for (; key_itr != end; ++key_itr) {
        // use first element of right range as key to sort into left range
        auto key = move_or_copy(*key_itr);

        // find position of key in left range
        auto i = start;
        while (!compare(key, *i) and i != key_itr) {
            ++i;
        }

        // shift elements from i to key_iter by one
        // TODO: if RNG is contiguous and value_type is trivially
        // relocatable then use memcpy
        if (i != key_itr) {
            auto tmp{move_or_copy(*i)};

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
}

template <class Ordering>
struct _adapter
{
    struct type;
};

template <class Ordering>
using adapter = _adapter<Ordering>::type;

namespace _cpo
{
inline constexpr struct _fn
{
    template <class Ordering = ordering::ascending>
    requires(!ranges::range<Ordering> and !ranges::view_<Ordering>)
    static constexpr auto operator()(Ordering&& ordering = {})
    {
        return adapter<Ordering>{std::forward<Ordering>(ordering)};
    }

    template <ranges::forward_range Range, class Ordering = ordering::ascending>
    requires(unifex::tag_invocable<_fn, Range, Ordering>)
    static constexpr auto operator()(Range&& range, Ordering&& ordering = {})
        noexcept(unifex::is_nothrow_tag_invocable_v<_fn, decltype(range), Ordering>)
            -> unifex::tag_invoke_result_t<_fn, Range, Ordering>
    {
        return tag_invoke(
            _fn{}, std::forward<Range>(range), std::forward<Ordering>(ordering));
    }

    template <class Itr, class Sentinel, class Ordering = ordering::ascending>
    requires(
        unifex::tag_invocable<_fn, Itr, Sentinel, Ordering> and
        ranges::forward_iterator<std::remove_cvref_t<Itr>> and
        ranges::sentinel_for<std::remove_cvref_t<Sentinel>, std::remove_cvref_t<Itr>>)
    static constexpr auto operator()(Itr&& start,
                                     Sentinel&& end,
                                     Ordering&& ordering = Ordering{})
        noexcept(unifex::is_nothrow_tag_invocable_v<_fn, Itr, Sentinel, Ordering>)
    {
        return tag_invoke(_fn{},
                          std::forward<Itr>(start),
                          std::forward<Sentinel>(end),
                          std::forward<Ordering>(ordering));
    }

private:
    template <class Itr, class Sentinel, class Ordering = ordering::ascending>
    requires(
        ranges::forward_iterator<std::remove_cvref_t<Itr>> and
        ranges::sentinel_for<std::remove_cvref_t<Sentinel>, std::remove_cvref_t<Itr>>)
    friend constexpr void tag_invoke(_fn /*unused*/,
                                     Itr&& start,
                                     Sentinel&& end,
                                     Ordering&& /**/ = {})
        noexcept(noexcept(_insertion_sort::algorithm(
            std::forward<Itr>(start),
            std::forward<Sentinel>(end),
            predicate_for_t<Ordering, std::iter_value_t<Itr>>{})))
    {
        _insertion_sort::algorithm(std::forward<Itr>(start),
                                   std::forward<Sentinel>(end),
                                   predicate_for_t<Ordering, std::iter_value_t<Itr>>{});
    }

    template <ranges::forward_range Range, class Ordering = ordering::ascending>
    friend constexpr auto tag_invoke(_fn /*unused*/, Range&& range, Ordering&& /**/ = {})
        noexcept(noexcept(_insertion_sort::algorithm(
            begin(range),
            end(range),
            predicate_for_t<Ordering, ranges::range_value_t<Range>>{})))
            -> decltype(std::forward<Range>(range))
    {
        _insertion_sort::algorithm(
            begin(range),
            end(range),
            predicate_for_t<Ordering, ranges::range_value_t<Range>>{});
        return std::forward<Range>(range);
    }
} insertion_sort;
} // namespace _cpo
} // namespace _insertion_sort

using _insertion_sort::_cpo::insertion_sort;

namespace _insertion_sort
{

template <class Compare>
struct _adapter<Compare>::type
{
    [[no_unique_address]] Compare compare_;

    template <class Adapter>
    requires std::same_as<std::remove_cvref_t<Adapter>, type>
    friend constexpr auto operator|(ranges::forward_range auto&& range, Adapter&& self)
    {
        return tag_invoke(unifex::tag_t<insertion_sort>{},
                          std::forward<decltype(range)>(range),
                          std::forward_like<Adapter>(self.compare_));
    }
};

} // namespace _insertion_sort
inline constexpr auto insertion_sort_descending = insertion_sort(ordering::descending{});
inline constexpr auto insertion_sort_ascending = insertion_sort(ordering::ascending{});

} // namespace algo
