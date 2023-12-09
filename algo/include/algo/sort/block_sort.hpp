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

#include "insertion_sort.hpp"

#include <iterator>
#include <range/v3/all.hpp>
#include <range/v3/range/access.hpp>
#include <range/v3/view/chunk.hpp>

#include <algorithm>
#include <bit>
#include <type_traits>
// #include <exception>

namespace algo
{

namespace _block_sort
{

constexpr auto for_each_block(auto&& begin,
                              auto&& size,
                              auto&& denominator,
                              auto&& decimal_step,
                              auto&& numerator_step,
                              auto&& fn)
{
    using diff_t = std::remove_cvref_t<decltype(decimal_step)>;
    using size_t = std::remove_cvref_t<decltype(size)>;
    diff_t decimal = 0;
    diff_t numerator = 0;
    diff_t start = 0;

    while (size_t(decimal) < size) {
        start = decimal;
        decimal += decimal_step;
        numerator += numerator_step;
        if (numerator >= denominator) {
            numerator -= denominator;
            ++decimal;
        }
        if (size_t(decimal) > size) {
            decimal = diff_t(size);
        }
        fn(ranges::subrange(begin + start, begin + decimal));
    }
}

constexpr auto for_each_block_pair(auto&& begin,
                                   auto&& size,
                                   auto&& denominator,
                                   auto&& decimal_step,
                                   auto&& numerator_step,
                                   auto&& fn)
{
    using diff_t = std::remove_cvref_t<decltype(decimal_step)>;
    using size_t = std::remove_cvref_t<decltype(size)>;
    diff_t decimal = 0;
    diff_t numerator = 0;
    diff_t start = 0;

    auto advance = [&]() {
        start = decimal;
        decimal += decimal_step;
        numerator += numerator_step;
        if (numerator >= denominator) {
            numerator -= denominator;
            ++decimal;
        }
        if (size_t(decimal) > size) {
            decimal = diff_t(size);
        }
    };

    while (size_t(decimal) < size) {
        advance();
        auto a = ranges::subrange(begin + start, begin + decimal);
        advance();
        auto b = ranges::subrange(begin + start, begin + decimal);
        fn(a, b);
    }
}

constexpr auto for_each_level(auto&& rng, size_t min_level, auto&& fn)
{
    using diff_t =
        ranges::range_difference_t<std::remove_cvref_t<decltype(rng)>>;
    using size_t =
        ranges::range_size_t<std::remove_cvref_t<decltype(rng)>>;
    auto size = ranges::size(rng);

    diff_t denominator =
        diff_t(std::bit_floor(ranges::size(rng)) / min_level);
    diff_t decimal_step = diff_t(size / size_t(denominator));
    diff_t numerator_step = diff_t(size % size_t(denominator));

    auto next_level = [&]() {
        decimal_step += decimal_step;
        numerator_step += numerator_step;
        if (numerator_step >= denominator) {
            numerator_step -= denominator;
            ++decimal_step;
        }
        if (size_t(decimal_step) > size) {
            decimal_step = diff_t(size);
        }
    };

    for (; size_t(decimal_step) < size; next_level()) {
        for_each_block_pair(ranges::begin(rng),
                            size,
                            denominator,
                            decimal_step,
                            numerator_step,
                            fn);
    }
}
constexpr auto for_first_level(auto&& rng, size_t min_level, auto&& fn)
{
    using diff_t =
        ranges::range_difference_t<std::remove_cvref_t<decltype(rng)>>;
    using size_t =
        ranges::range_size_t<std::remove_cvref_t<decltype(rng)>>;

    size_t size = ranges::size(rng);
    diff_t denominator =
        diff_t(std::bit_floor(ranges::size(rng)) / min_level);
    diff_t decimal_step = diff_t(size / size_t(denominator));
    diff_t numerator_step = diff_t(size % size_t(denominator));

    for_each_block(ranges::begin(rng),
                   size,
                   denominator,
                   decimal_step,
                   numerator_step,
                   std::forward<decltype(fn)>(fn));
}

constexpr void merge_cache(auto&& cache,
                           auto&& litr,
                           auto&& lend,
                           auto&& ritr,
                           auto&& rend,
                           auto const& cmp)
{
    ranges::swap_ranges(litr, lend, cache);
    auto dest = litr;
    lend = cache + std::distance(litr, lend);
    litr = cache;

    while (litr != lend and ritr != rend) {
        if (cmp(*litr, *ritr)) {
            *dest = std::move(*litr);
            ++litr;
        }
        else {
            *dest = std::move(*ritr);
            ++ritr;
        }
        ++dest;
    }
    if (litr != lend) {
        ranges::swap_ranges(litr, lend, dest);
    }
}

constexpr void merge_inplace(
    auto&& litr, auto&& lend, auto&& ritr, auto&& rend, auto const& cmp)
{
    if (std::distance(litr, lend) == 0|| std::distance(ritr, rend) == 0) {
        return;
    }

    while(true) {

    }
}

constexpr auto setup_cache(auto& cache, auto& range_size)
{
    auto cache_size = size_t((range_size + 1) / 2);

    try {
        cache.resize(cache_size);
    }
    catch (std::bad_alloc& err) {
        try {
            cache_size = size_t(std::sqrt(cache_size) + 1);
            cache.resize(cache_size);
        }
        catch (std::bad_alloc& err) {
            try {
                cache_size = 512;
                cache.resize(cache_size);
            }
            catch (std::bad_alloc& err) {
                cache_size = 0;
            }
        }
    }
    return cache_size;
}

constexpr auto sort_small_4(auto&& rng, auto&& size, auto&& cmp)
{
    if (size < 4) {
        auto a = std::begin(rng);
        auto b = a + 1;
        if (size == 3) {
            auto c = a + 2;
            if (cmp(*b, *a)) {
                std::iter_swap(a, b);
            }
            if (cmp(*c, *b)) {
                std::iter_swap(c, b);
                if (cmp(*b, *a)) {
                    std::iter_swap(a, b);
                }
            }
        }
        else if (size == 2) {
            if (cmp(*b, *a)) {
                std::iter_swap(a, b);
            }
        }
    }
}
} // namespace _block_sort

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void block_sort(RNG& rng, CMP cmp = CMP{}) // NOLINT
{
    size_t size = std::size(rng);
    std::vector<ranges::range_value_t<RNG>> cache{};

    if (size < 4) {
        _block_sort::sort_small_4(rng, size, cmp);
        return;
    }

    _block_sort::for_first_level(
        rng, 16, [&](auto&& block) { insertion_sort(block, cmp); });

    if (size < 16) {
        return;
    }

    size_t _ = _block_sort::setup_cache(cache, size);

    _block_sort::for_each_level(rng, 16, [&](auto&& a, auto&& b) {
        auto a_begin = ranges::begin(a);
        auto a_last = ranges::begin(a) +
                      ranges::range_difference_t<RNG>(ranges::size(a) - 1);
        auto b_begin = ranges::begin(b);
        auto b_last = ranges::begin(b) +
                      ranges::range_difference_t<RNG>(ranges::size(b) - 1);

        if (cmp(*b_last, *a_begin)) {
            ranges::rotate(ranges::subrange(a_begin, ++b_last), b_begin);
        }
        else if (cmp(*b_begin, *a_last)) {
            _block_sort::merge_cache(std::begin(cache),
                                     a_begin,
                                     ++a_last,
                                     b_begin,
                                     ++b_last,
                                     cmp);
        }
    });
}
} // namespace algo
