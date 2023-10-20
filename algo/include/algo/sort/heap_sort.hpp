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

#include <ranges>
namespace algo
{

namespace _heap_sort
{

constexpr auto i_parent(auto x)
{
    if (x == 0) {
        // TODO: use proper exception.
        throw std::exception();
    }
    return (x - 1) / 2;
}

constexpr auto i_left_child(auto x) noexcept
{
    return 2 * x + 1;
}

constexpr auto i_right_child(auto x) noexcept
{
    return 2 * x + 2;
}

inline auto leaf_search(auto const& rng, auto i, auto end, auto const& cmp)
{
    auto j = i;
    auto lc = i_left_child(j);
    auto rc = lc + 1;
    while (rc < end) {
        if (!cmp(rng[rc], rng[lc])) {
            j = rc;
        }
        else {
            j = lc;
        }
        lc = i_left_child(j);
        rc = lc + 1;
    }

    lc = i_left_child(j);
    if (lc < end) {
        j = lc;
    }
    return j;
}

inline void sift_down(
    auto& rng,
    std::ranges::range_size_t<std::remove_cvref_t<decltype(rng)>> i,
    auto end,
    auto const& cmp)
{
    using std::swap;
    auto j = leaf_search(rng, i, end, cmp);
    while (!cmp(rng[i], rng[j]) and j != 0) {
        j = i_parent(j);
    }
    while (j > i) {
        swap(rng[i], rng[j]);
        j = i_parent(j);
    }
};

inline void heapify(auto& rng, auto size, auto const& cmp)
{
    auto start = i_parent(size + 1);

    while (start > 0) {
        --start;
        sift_down(rng, start, size, cmp);
    }
};
} // namespace _heap_sort

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void heap_sort(RNG& rng, CMP cmp = CMP{})
{
    using _heap_sort::heapify;
    using _heap_sort::sift_down;
    using std::swap;

    auto end = std::size(rng);
    heapify(rng, end, cmp);

    while (end > 1) {
        --end;
        swap(rng[end], rng[0]);
        sift_down(rng, 0, end, cmp);
    }
}
} // namespace algo
