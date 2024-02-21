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
#include <algo/partition.hpp>

#include <range/v3/view/subrange.hpp>
#include <ranges>
#include <unifex/tag_invoke.hpp>

#include <algo/prelude.hpp>

namespace algo
{

namespace _quick_sort
{

template <auto const& partition_algorithm>
struct Algorithm
{
    constexpr static void operator()(auto first, auto last, auto const& cmp)
    requires(std::random_access_iterator<std::remove_cvref_t<decltype(first)>> and
             std::random_access_iterator<std::remove_cvref_t<decltype(last)>>)
    {
        auto size = ranges::distance(first, last);
        if (size < 2) {
            return;
        }
        auto const pivot = *(first + ((size + 1) / 2));

        auto middle1 = get<0>(
            partition_algorithm(ranges::subrange(first, last),
                                [&, pivot](auto const& elm) { return cmp(elm, pivot); }));

        auto middle2 = get<0>(partition_algorithm(
            ranges::subrange(middle1, last),
            [&, pivot](auto const& elm) { return !cmp(pivot, elm); }));

        // all equal elems
        if (middle1 != last) {
            Algorithm::operator()(first, middle1, cmp);
        }

        if (middle2 != first) {
            Algorithm::operator()(middle2, last, cmp);
        }
    }
};

template <std::ranges::contiguous_range RNG, class Partition, class CMP = std::less<>>
auto algorithm(RNG rng,
               Partition const& /*partition_algorithm*/,
               CMP const& cmp = {}) -> RNG
{
    static constexpr auto partition_algorithm = Partition{};
    constexpr auto alg = Algorithm<partition_algorithm>{};
    if (!ranges::empty(rng)) {
        alg(std::begin(rng), std::end(rng), cmp);
    }
    return rng;
}

template <class Ordering, class Paritition>
struct _adapter final
{
    struct type;
};

template <class Ordering, class Partition>
using adapter = _adapter<std::remove_cvref_t<Ordering>, Partition>::type;

namespace _cpo
{
inline constexpr struct _fn
{
    static constexpr auto operator()(auto const& ordering)
    {
        return adapter<decltype(ordering), unifex::tag_t<hoare_partition>>{
            hoare_partition};
    }

    template <class Ordering>
    static constexpr auto operator()(ranges::contiguous_range auto&& range,
                                     Ordering&& ordering = ordering::ascending{})
    {
        return tag_invoke(_fn{}, FWD(range), FWD(ordering), hoare_partition);
    }

    template <class Partition>
    static constexpr auto operator()(auto const& ordering,
                                     Partition const& partition_algorithm)
    {
        return adapter<decltype(ordering), Partition>{partition_algorithm};
    }

    template <class Ordering, class Partition>
    static constexpr auto operator()(ranges::contiguous_range auto&& range,
                                     Partition const& partition_algorithm,
                                     Ordering&& ordering = ordering::ascending{})
    {
        return tag_invoke(_fn{}, FWD(range), FWD(ordering), partition_algorithm);
    }

private:
    template <class Ordering, class Partition>
    friend constexpr auto tag_invoke(_fn const& /*unused*/,
                                     ranges::contiguous_range auto&& range,
                                     Ordering const& /*ordering*/,
                                     Partition const& partition_algorithm)
    {
        return algorithm(FWD(range),
                         partition_algorithm,
                         predicate_for_t<Ordering, RNG_VALUE_T(range)>{});
    }
} quick_sort;
} // namespace _cpo
} // namespace _quick_sort

using _quick_sort::_cpo::quick_sort;

namespace _quick_sort
{
template <class Ordering, class Partition>
struct _adapter<Ordering, Partition>::type final
{
    Partition const& partition_algorithm; // NOLINT

    template <class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(ranges::contiguous_range auto&& range,
                                    Adapter&& adapter)
    {
        return tag_invoke(quick_sort,
                          FWD(range),
                          Ordering{},
                          std::forward<Adapter>(adapter).partition_algorithm);
    }

    constexpr auto operator()(this auto&& self, ranges::contiguous_range auto&& range)
    {
        return tag_invoke(
            quick_sort, FWD(range), Ordering{}, FWD(self).partition_algorithm);
    }

    template <class Partition2>
    constexpr auto operator()(Partition2 const& partition_algorithm2) const
    {
        return type{partition_algorithm2};
    }

    template <class Partition2>
    constexpr auto operator()(ranges::contiguous_range auto&& range,
                              Partition2 const& partition_algorithm2) const
    {
        return tag_invoke(quick_sort, FWD(range), Ordering{}, partition_algorithm2);
    }
};
} // namespace _quick_sort
} // namespace algo

#include <algo/prologue.hpp>
