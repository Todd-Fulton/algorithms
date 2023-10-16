#pragma once

#include <ranges>
#include <vector>

namespace algo
{

template <std::ranges::contiguous_range RNG, class CMP = std::less<>>
void merge_sort(RNG& rng, CMP cmp = CMP{})
{

    // TODO: Convert to loop based algorithm.
    
    auto merge = [&cmp](auto dest, auto litr, auto lend, auto ritr, auto rend) {

        while(litr != lend and ritr != rend) {
            if(cmp(*litr, *ritr)) {
                *dest = std::move(*litr);
                ++litr;
            } else {
                *dest = std::move(*ritr);
                ++ritr;
            }
            ++dest;
        }

        while(litr != lend) {
            *dest = std::move(*litr);
            ++dest, ++litr;
        }

        while(ritr != rend) {
            *dest = std::move(*ritr);
            ++dest, ++ritr;
        }
    };

    auto top_down_split_merge = [&merge](this auto&& self, auto src, auto dst, long size) {
        if (size <= 1) { return ;}
        
        auto sz_left = size / 2 + (size % 2);
        auto sz_right = size / 2;

        auto sbegin_left{src};
        auto sbegin_right{src + sz_left};

        auto dbegin_left{dst};
        auto dend_left{dst + sz_left};
        auto dbegin_right{dst + sz_left};
        auto dend_right{dst + size};


        self(dbegin_left, sbegin_left, sz_left);
        self(dbegin_right, sbegin_right, sz_right);
        merge(src, dbegin_left, dend_left, dbegin_right, dend_right);
    };
    
    std::vector<std::ranges::range_value_t<RNG>> dst{rng};
    auto src_itr = std::begin(rng);
    auto dst_itr = std::begin(dst);

    top_down_split_merge(src_itr, dst_itr, static_cast<long>(std::size(rng)));

}
} // namespace algo
