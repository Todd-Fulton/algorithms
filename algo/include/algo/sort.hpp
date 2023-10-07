#pragma once

#include <ranges>

namespace algo
{

/**
 * @brief Implements insertion sort for a range satisfying `forward
 * iterable`.
 *
 * @param rng A range at least satisfying `std::ranges::forward_range`
 * @param cmp A comparison function. Use less for ascending order and
 * greater for descending order.
 */
template <std::ranges::forward_range RNG, class CMP = std::less<>>
requires(
    std::is_nothrow_move_assignable_v<std::ranges::range_value_t<RNG>> and
    std::is_default_constructible_v<std::ranges::range_value_t<RNG>>)
void insertion_sort(RNG& rng, CMP cmp = CMP{})
{
    using std::swap;
    if (rng.begin() == rng.end()) {
        return;
    }

    // assume range of size 1 on left is sorted
    auto key_itr = rng.begin();
    ++key_itr;
    for (; key_itr != rng.end(); ++key_itr) {
        // use first element of right range as key to sort into left range
        auto key = std::move(*key_itr);

        // find position of key in left range
        auto i = rng.begin();
        while (!cmp(key, *i) and i != key_itr) {
            ++i;
        }

        // shift elements from i to key_iter by one
        // TODO: if RNG is contiguous and value_type is trivially
        // relocatable then use memcpy
        if (i != key_itr) {
            std::ranges::range_value_t<RNG> tmp;
            swap(*i, tmp);

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
} // namespace algo
