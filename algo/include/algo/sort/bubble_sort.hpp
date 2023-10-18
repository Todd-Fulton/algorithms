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

#include <ranges>
namespace algo
{

template <std::ranges::forward_range RNG, class CMP = std::greater<>>
void bubble_sort(RNG& rng, CMP cmp = CMP{})
{
    auto begin = std::begin(rng);
    auto end = std::end(rng);
    auto snd = begin;
    ++snd;
    auto a = begin;
    auto b = snd;

    while (end != snd) {
        while (b != end) {
            if (cmp(*a, *b)) {
                using std::swap;
                swap(*a, *b);
            }
            ++a;
            ++b;
        }
        end = a;
        a = begin;
        b = snd;
    }
}

} // namespace algo
