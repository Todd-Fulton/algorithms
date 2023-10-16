#pragma once

#include <ranges>

namespace algo
{

template <std::ranges::forward_range RNG, class CMP = std::equal_to<>>
auto linear_search(RNG& rng,
                   std::ranges::range_value_t<RNG> const& key,
                   CMP cmp = CMP{})
    -> std::ranges::iterator_t<RNG>
{
    auto itr = std::begin(rng);
    for (; itr != std::end(rng); ++itr) {
        if (cmp(*itr, key)) {
            break;
        }
    }
    return itr;
}
} // namespace algo
