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

#include <algo/prelude.hpp>

namespace algo
{
namespace _bubble_sort
{

auto algorithm(ranges::forward_range auto rng,
               auto const& cmp) -> std::remove_reference_t<decltype(rng)>
{
    auto begin = ranges::begin(rng);
    auto end = ranges::end(rng);
    auto snd = begin;
    ++snd;
    auto a = begin;
    auto b = snd;

    while (end != snd) {
        while (b != end) {
            if (cmp(*b, *a)) {
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

template <class Ordering>
struct _adapter final
{
    struct type;
};

template <class Ordering>
using adapter = _adapter<std::decay_t<Ordering>>::type;

namespace _cpo
{
struct _fn final
{
    template <class Ordering = ordering::ascending>
    static constexpr auto operator()([[maybe_unused]] auto const& ordering)
    {
        return _bubble_sort::adapter<Ordering>{};
    }

    template <class Ordering = ordering::ascending>
    static constexpr auto operator()(ranges::forward_range auto&& range,
                                     auto const& ordering = Ordering{})
    {
        return tag_invoke(_fn{}, FWD(range), ordering);
    }

private:
    template <class Ordering>
    friend auto tag_invoke(_fn const& /*_*/,
                           ranges::forward_range auto&& range,
                           Ordering const& /*_*/)
    {
        return algorithm(FWD(range), predicate_for_t<Ordering, RNG_VALUE_T(range)>{});
    }
};

} // namespace _cpo
} // namespace _bubble_sort

constexpr auto bubble_sort = _bubble_sort::_cpo::_fn{};

namespace _bubble_sort
{
template <class Ordering>
struct _adapter<Ordering>::type final
{
    friend constexpr auto operator|(ranges::forward_range auto&& range,
                                    [[maybe_unused]] type const& _)
    {
        return bubble_sort(FWD(range), Ordering{});
    }
};

} // namespace _bubble_sort

constexpr auto bubble_sort_decending = bubble_sort(ordering::descending{});
constexpr auto bubble_sort_ascending = bubble_sort(ordering::ascending{});

} // namespace algo

#include <algo/prologue.hpp>
