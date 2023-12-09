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

#include <range/v3/algorithm/for_each.hpp>
#include <ranges>
#include <vector>

namespace algo
{

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void merge_sort(RNG& rng, CMP cmp = CMP{})
{

    // TODO: Convert to loop based algorithm.
    auto size = std::size(rng);
    // NOTE: (A.1) This is too limiting or just wrong, not limiting enough.
    if (size >
        std::numeric_limits<std::ranges::range_difference_t<RNG>>::max()) {
        // TODO: use proper exception.
        throw std::exception();
    }

    auto merge =
        [&cmp](auto dest, auto litr, auto lend, auto ritr, auto rend) {
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

            while (litr != lend) {
                *dest = std::move(*litr);
                ++dest, ++litr;
            }

            while (ritr != rend) {
                *dest = std::move(*ritr);
                ++dest, ++ritr;
            }
        };

    auto top_down_split_merge =
        [&merge](auto const& self,
                 auto src,
                 auto dst,
                 std::ranges::range_difference_t<RNG> size) {
            if (size <= 1) {
                return;
            }

            auto sz_left = size / 2 + (size % 2);
            auto sz_right = size / 2;

            auto sbegin_left{src};
            auto sbegin_right{src + sz_left};

            auto dbegin_left{dst};
            auto dend_left{dst + sz_left};
            auto dbegin_right{dst + sz_left};
            auto dend_right{dst + size};

            self(self, dbegin_left, sbegin_left, sz_left);
            self(self, dbegin_right, sbegin_right, sz_right);
            merge(src, dbegin_left, dend_left, dbegin_right, dend_right);
        };

    std::vector<std::ranges::range_value_t<RNG>> dst;
    dst.reserve(size);
    ranges::for_each(rng,
                     [&](auto& x) { dst.emplace_back(std::move(x)); });
    auto src_itr = std::begin(rng);
    auto dst_itr = std::begin(dst);

    // NOTE: (A.1) The precondition on the size of the range rng depends
    // on the location in memory of the start dst. If the address of the
    // begining of dst memory storage is X, then X + size(rng) should be
    // less than or equal to the maximum addressible limit of the system.
    top_down_split_merge(
        top_down_split_merge,
        src_itr,
        dst_itr,
        static_cast<std::ranges::range_difference_t<RNG>>(size));
}
} // namespace algo
