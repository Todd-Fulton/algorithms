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
namespace _bubble_sort
{

template <ranges::forward_range RNG, class CMP = std::greater<>>
void bubble_sort(RNG&& rng, CMP cmp = CMP{})
{
    auto begin = ranges::begin(rng);
    auto end = ranges::end(rng);
    auto snd = begin;
    ++snd;
    auto a = begin;
    auto b = snd;

    while (end != snd) {
        while (b != end) {
            if (cmp(*a, *b)) {
                ranges::iter_swap(a, b);
            }
            ++a;
            ++b;
        }
        end = a;
        a = begin;
        b = snd;
    }
    return rng;
}

template <class Ordering, class T = void>
struct pred;

template <class T>
struct pred<ordering::ascending, T> : std::greater<T>
{};

template <class T>
struct pred<ordering::decending, T> : std::less<T>
{};

template <class COMPARE>
struct bubble_sort_adapter
{
    friend constexpr auto operator|(
        ranges::forward_range auto range,
        [[maybe_unused]] bubble_sort_adapter const& _)
    {
        return bubble_sort(
            std::move(range),
            pred<COMPARE, ranges::range_value_t<decltype(range)>>{});
    }
};

} // namespace _bubble_sort

constexpr struct
{
    template <class Ordering = ordering::ascending>
    static constexpr auto operator()(
        [[maybe_unused]] auto const& ordering = Ordering{})
    {
        return _bubble_sort::bubble_sort_adapter<Ordering>{};
    }

    template <class Ordering = ordering::ascending>
    static constexpr auto operator()(ranges::contiguous_range auto&& range,
                                     auto const& /*unused*/ = Ordering{})
    {
        return _bubble_sort::bubble_sort(
            std::forward<decltype(range)>(range),
            _bubble_sort::pred<Ordering,
                               ranges::range_value_t<decltype(range)>>{});
    }

    static constexpr auto operator()()
    {
        return _bubble_sort::bubble_sort_adapter<ordering::ascending>{};
    }

} bubble_sort;

constexpr auto bubble_sort_decending = bubble_sort(ordering::decending{});
constexpr auto bubble_sort_ascending = bubble_sort(ordering::ascending{});

} // namespace algo
