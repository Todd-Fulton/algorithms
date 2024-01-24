/***********************************************************
 WikiSort: a public domain implementation of "Block Sort"
 https://github.com/BonzaiThePenguin/WikiSort

 to run:
 clang++ -o WikiSort.x WikiSort.cpp -O3
 (or replace 'clang++' with 'g++')
 ./WikiSort.x
***********************************************************/

#pragma once

#include <algorithm>
#include <cassert>
#include <cmath>
#include <iterator>
#include <limits>

#include <range/v3/all.hpp>

namespace algo
{
namespace _block_sort
{

// structure to represent ranges within the array
template <typename Iterator>
struct Range
{
    Iterator start;
    Iterator end;

    constexpr Range() = default;

    Range(Iterator start, Iterator end)
        : start(start)
        , end(end)
    {
    }

    template <class RANGE>
    requires(ranges::range<std::remove_cvref_t<RANGE>>)
    constexpr explicit Range(RANGE&& range)
        : start{ranges::begin(range)}
        , end{ranges::end(range)}
    {
    }

    [[nodiscard]] std::size_t length() const
    {
        return std::distance(start, end);
    }
};

template <class RANGE>
Range(RANGE& range) -> Range<ranges::iterator_t<RANGE>>;

// toolbox functions used by the sorter

// 63 -> 32, 64 -> 64, etc.
// this comes from Hacker's Delight
auto Hyperfloor(auto value)
{
    for (std::size_t i = 1;
         i <= std::numeric_limits<
                  std::remove_cvref_t<decltype(value)>>::digits /
                  2;
         i <<= 1U) {
        value |= (value >> i);
    }
    return value - (value >> 1U);
}

// combine a linear search with a binary search to reduce the number of
// comparisons in situations where have some idea as to how many unique
// values there are and where the next value might be
template <typename RandomAccessIterator, typename T, typename Comparison>
RandomAccessIterator FindFirstForward(RandomAccessIterator first,
                                      RandomAccessIterator last,
                                      const T& value,
                                      Comparison compare,
                                      std::size_t unique)
{
    std::size_t size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    std::size_t skip = std::max(size / unique, std::size_t(1));

    RandomAccessIterator index;
    for (index = first + skip; compare(*(index - 1), value);
         index += skip) {
        if (index >= last - skip) {
            return std::lower_bound(index, last, value, compare);
        }
    }
    return std::lower_bound(index - skip, index, value, compare);
}

template <typename RandomAccessIterator, typename T, typename Comparison>
RandomAccessIterator FindLastForward(RandomAccessIterator first,
                                     RandomAccessIterator last,
                                     const T& value,
                                     Comparison compare,
                                     std::size_t unique)
{
    std::size_t size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    std::size_t skip = std::max(size / unique, std::size_t(1));

    RandomAccessIterator index;
    for (index = first + skip; !compare(value, *(index - 1));
         index += skip) {
        if (index >= last - skip) {
            return std::upper_bound(index, last, value, compare);
        }
    }
    return std::upper_bound(index - skip, index, value, compare);
}

template <typename RandomAccessIterator, typename T, typename Comparison>
RandomAccessIterator FindFirstBackward(RandomAccessIterator first,
                                       RandomAccessIterator last,
                                       const T& value,
                                       Comparison compare,
                                       std::size_t unique)
{
    std::size_t size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    std::size_t skip = std::max(size / unique, std::size_t(1));

    RandomAccessIterator index;
    for (index = last - skip;
         index > first && !compare(*(index - 1), value);
         index -= skip) {
        if (index < first + skip) {
            return std::lower_bound(first, index, value, compare);
        }
    }
    return std::lower_bound(index, index + skip, value, compare);
}

template <typename RandomAccessIterator, typename T, typename Comparison>
RandomAccessIterator FindLastBackward(RandomAccessIterator first,
                                      RandomAccessIterator last,
                                      const T& value,
                                      Comparison compare,
                                      std::size_t unique)
{
    std::size_t size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    std::size_t skip = std::max(size / unique, std::size_t(1));

    RandomAccessIterator index;
    for (index = last - skip;
         index > first && compare(value, *(index - 1));
         index -= skip) {
        if (index < first + skip) {
            return std::upper_bound(first, index, value, compare);
        }
    }
    return std::upper_bound(index, index + skip, value, compare);
}

template <typename BidirectionalIterator, typename Comparison>
void InsertionSort(BidirectionalIterator first,
                   BidirectionalIterator last,
                   Comparison compare)
{
    typedef
        typename std::iterator_traits<BidirectionalIterator>::value_type T;
    if (first == last) {
        return;
    }

    for (BidirectionalIterator cur = first + 1; cur != last; ++cur) {
        BidirectionalIterator sift = cur;
        BidirectionalIterator sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for
        // an element already positioned correctly.
        if (compare(*sift, *sift_1)) {
            T tmp = *sift;
            do {
                *sift-- = *sift_1;
            } while (sift != first && compare(tmp, *--sift_1));
            *sift = tmp;
        }
    }
}

// merge operation using an external buffer
template <typename RandomAccessIterator1,
          typename RandomAccessIterator2,
          typename Comparison>
void MergeExternal(RandomAccessIterator1 first1,
                   RandomAccessIterator1 last1,
                   RandomAccessIterator1 first2,
                   RandomAccessIterator1 last2,
                   RandomAccessIterator2 cache,
                   Comparison compare)
{
    // A fits into the cache, so use that instead of the internal buffer
    RandomAccessIterator2 A_index = cache;
    RandomAccessIterator2 A_last = cache + std::distance(first1, last1);
    RandomAccessIterator1 B_index = first2;
    RandomAccessIterator1 B_last = last2;
    RandomAccessIterator1 insert_index = first1;

    if (last2 - first2 > 0 && last1 - first1 > 0) {
        while (true) {
            if (!compare(*B_index, *A_index)) {
                *insert_index = *A_index;
                ++A_index;
                ++insert_index;
                if (A_index == A_last) {
                    break;
                }
            }
            else {
                *insert_index = *B_index;
                ++B_index;
                ++insert_index;
                if (B_index == B_last) {
                    break;
                }
            }
        }
    }

    // copy the remainder of A into the final array
    ranges::copy(A_index, A_last, insert_index);
}

// merge operation using an internal buffer
template <typename RandomAccessIterator, typename Comparison>
void MergeInternal(RandomAccessIterator first1,
                   RandomAccessIterator last1,
                   RandomAccessIterator first2,
                   RandomAccessIterator last2,
                   RandomAccessIterator buffer,
                   Comparison compare)
{
    // whenever we find a value to add to the final array, swap it with the
    // value that's already in that spot when this algorithm is finished,
    // 'buffer' will contain its original contents, but in a different
    // order
    RandomAccessIterator A_index = buffer;
    RandomAccessIterator B_index = first2;
    RandomAccessIterator A_last = buffer + std::distance(first1, last1);
    RandomAccessIterator B_last = last2;
    RandomAccessIterator insert_index = first1;

    if (last2 - first2 > 0 && last1 - first1 > 0) {
        while (true) {
            if (!compare(*B_index, *A_index)) {
                std::iter_swap(insert_index, A_index);
                ++A_index;
                ++insert_index;
                if (A_index == A_last) {
                    break;
                }
            }
            else {
                std::iter_swap(insert_index, B_index);
                ++B_index;
                ++insert_index;
                if (B_index == B_last) {
                    break;
                }
            }
        }
    }

    // BlockSwap
    std::swap_ranges(A_index, A_last, insert_index);
}

// merge operation without a buffer
template <typename RandomAccessIterator, typename Comparison>
void MergeInPlace(RandomAccessIterator first1,
                  RandomAccessIterator last1,
                  RandomAccessIterator first2,
                  RandomAccessIterator last2,
                  Comparison compare)
{
    if (last1 - first1 == 0 || last2 - first2 == 0) {
        return;
    }

    /*
     this just repeatedly binary searches into B and rotates A into
     position. the paper suggests using the 'rotation-based Hwang and Lin
     algorithm' here, but I decided to stick with this because it had
     better situational performance

     (Hwang and Lin is designed for merging subarrays of very different
     sizes, but WikiSort almost always uses subarrays that are roughly the
     same size)

     normally this is incredibly suboptimal, but this function is only
     called when none of the A or B blocks in any subarray contained 2√A
     unique values, which places a hard limit on the number of times this
     will ACTUALLY need to binary search and rotate.

     according to my analysis the worst case is √A rotations performed on
     √A items once the constant factors are removed, which ends up being
     O(n)

     again, this is NOT a general-purpose solution – it only works well in
     this case! kind of like how the O(n^2) insertion sort is used in some
     places
     */

    while (true) {
        // find the first place in B where the first item in A needs to be
        // inserted
        RandomAccessIterator mid =
            std::lower_bound(first2, last2, *first1, compare);

        // rotate A into place
        std::size_t amount = mid - last1;
        std::rotate(first1, last1, mid);
        if (last2 == mid) {
            break;
        }

        // calculate the new A and B ranges
        first2 = mid;
        first1 += amount;
        last1 = first2;
        first1 = std::upper_bound(first1, last1, *first1, compare);
        if (std::distance(first1, last1) == 0) {
            break;
        }
    }
}

// calculate how to scale the index value to the range within the array
// the bottom-up merge sort only operates on values that are powers of two,
// so scale down to that power of two, then use a fraction to scale back
// again
class Iterator
{
    std::size_t size, power_of_two;
    std::size_t decimal{}, numerator{}, denominator;
    std::size_t decimal_step, numerator_step;

public:
    Iterator(std::size_t size, std::size_t min_level)
        : size(size)
        , power_of_two(Hyperfloor(size))
        , denominator(power_of_two / min_level)
        , decimal_step(size / denominator)
        , numerator_step(size % denominator)
    {
    }

    void begin()
    {
        numerator = decimal = 0;
    }

    template <ranges::random_access_range RANGE>
    auto nextRange(RANGE& range) -> Range<ranges::iterator_t<RANGE>>
    {
        std::size_t start = decimal;

        decimal += decimal_step;
        numerator += numerator_step;
        if (numerator >= denominator) {
            numerator -= denominator;
            ++decimal;
        }

        return Range<ranges::iterator_t<RANGE>>(
            ranges::begin(range) + start, ranges::begin(range) + decimal);
    }

    [[nodiscard]] bool finished() const
    {
        return decimal >= size;
    }

    bool nextLevel()
    {
        decimal_step += decimal_step;
        numerator_step += numerator_step;
        if (numerator_step >= denominator) {
            numerator_step -= denominator;
            ++decimal_step;
        }

        return decimal_step < size;
    }

    [[nodiscard]] std::size_t length() const
    {
        return decimal_step;
    }
};

template <class RandomAccessIterator>
struct InternalBuffers
{

    // as an optimization, we really only need to pull out the
    // internal buffers once for each level of merges after that we
    // can reuse the same buffers over and over, then redistribute
    // it when we're finished with this level
    std::size_t block_size;
    std::size_t buffer_size;
    std::size_t find;
    std::size_t count = 0;
    std::size_t pull_index = 0;
    Range<RandomAccessIterator> buffer1{};
    Range<RandomAccessIterator> buffer2{};
    RandomAccessIterator buffer_index;
    RandomAccessIterator last;
    struct PullItr
    {
        RandomAccessIterator from{}, to{};
        std::size_t count{};
        Range<RandomAccessIterator> range;

        constexpr explicit PullItr(
            ranges::random_access_range auto&& range)
            : range{range}
        {
        }
    };
    std::array<PullItr, 2> pull_itrs{};
    bool find_separately = false;

    constexpr explicit InternalBuffers(
        ranges::random_access_range auto&& range,
        std::size_t length,
        std::size_t cache_size)
        : block_size{std::size_t(std::sqrt(length))}
        , buffer_size{length / block_size + 1}
        , find{buffer_size + buffer_size}
        , buffer1{range}
        , buffer2{range}
        , pull_itrs{{PullItr(range), PullItr(range)}}
    {
        // find two internal buffers of size 'buffer_size' each
        // let's try finding both buffers at the same time from a
        // single A or B subarray
        if (block_size <= cache_size) {
            // if every A block fits into the cache then we won't need
            // the second internal buffer, so we really only need to
            // find 'buffer_size' unique values
            find = buffer_size;
        }
        else if (find > length) {
            // we can't fit both buffers into the same A or B subarray,
            // so find two buffers separately
            find = buffer_size;
            find_separately = true;
        }
    }

    // just store information about where the values will be
    // pulled from and to, as well as how many values there
    // are, to create the two internal buffers
    constexpr auto pull(auto&& _to, auto&& start, auto&& end)
    {
        pull_itrs[pull_index].range =
            Range<RandomAccessIterator>(start, end);
        pull_itrs[pull_index].count = count;
        pull_itrs[pull_index].from = buffer_index;
        pull_itrs[pull_index].to = _to;
    }

    constexpr auto unique(auto&& subrangeA,
                          auto&& subrangeB,
                          std::size_t cache_size,
                          auto const& compare)
    {
        return unique_a(subrangeA, subrangeB, cache_size, compare) ||
               unique_b(subrangeA, subrangeB, cache_size, compare);
    }

    constexpr auto unique_a(auto&& subrangeA,
                            auto&& subrangeB,
                            std::size_t cache_size,
                            auto const& compare)
    {
        // check A for the number of unique values we need to fill
        // an internal buffer these values will be pulled out to
        // the start of A
        for (last = subrangeA.start, count = 1; count < find;
             last = buffer_index, ++count) {
            buffer_index = FindLastForward(
                last + 1, subrangeA.end, *last, compare, find - count);
            if (buffer_index == subrangeA.end) {
                break;
            }
            assert(buffer_index < subrangeA.end);
        }
        buffer_index = last;

        if (count >= buffer_size) {
            // keep track of the range within the array where we'll
            // need to "pull out" these values to create the
            // internal buffer
            pull(subrangeA.start, subrangeA.start, subrangeB.end);
            pull_index = 1;

            if (count == buffer_size + buffer_size) {
                // we were able to find a single contiguous section
                // containing 2√A unique values, so this section
                // can be used to contain both of the internal
                // buffers we'll need
                buffer1 = Range<RandomAccessIterator>(
                    subrangeA.start, subrangeA.start + buffer_size);
                buffer2 = Range<RandomAccessIterator>(
                    subrangeA.start + buffer_size,
                    subrangeA.start + count);
                return true;
            }
            else if (find == buffer_size + buffer_size) {
                // we found a buffer that contains at least √A
                // unique values, but did not contain the full 2√A
                // unique values, so we still need to find a second
                // separate buffer of at least √A unique values
                buffer1 = Range<RandomAccessIterator>(
                    subrangeA.start, subrangeA.start + count);
                find = buffer_size;
            }
            else if (block_size <= cache_size) {
                // we found the first and only internal buffer that
                // we need, so we're done!
                buffer1 = Range<RandomAccessIterator>(
                    subrangeA.start, subrangeA.start + count);
                return true;
            }
            else if (find_separately) {
                // found one buffer, but now find the other one
                buffer1 = Range<RandomAccessIterator>(
                    subrangeA.start, subrangeA.start + count);
                find_separately = false;
            }
            else {
                // we found a second buffer in an 'A' subarray
                // containing √A unique values, so we're done!
                buffer2 = Range<RandomAccessIterator>(
                    subrangeA.start, subrangeA.start + count);
                return true;
            }
        }
        else if (pull_index == 0 && count > buffer1.length()) {
            // keep track of the largest buffer we were able to
            // find
            buffer1 = Range<RandomAccessIterator>(subrangeA.start,
                                                  subrangeA.start + count);
            pull(subrangeA.start, subrangeA.start, subrangeB.end);
        }

        return false;
    }

    constexpr auto unique_b(auto&& subrangeA,
                            auto&& subrangeB,
                            std::size_t cache_size,
                            auto const& compare)
    {
        // check B for the number of unique values we need to fill
        // an internal buffer these values will be pulled out to
        // the end of B
        for (last = subrangeB.end - 1, count = 1; count < find;
             last = buffer_index - 1, ++count) {
            buffer_index = FindFirstBackward(
                subrangeB.start, last, *last, compare, find - count);
            if (buffer_index == subrangeB.start) {
                break;
            }
            assert(buffer_index > subrangeB.start);
        }
        buffer_index = last;

        if (count >= buffer_size) {
            // keep track of the range within the array where we'll
            // need to "pull out" these values to create the
            // internal buffer
            pull(subrangeB.end, subrangeA.start, subrangeB.end);
            pull_index = 1;

            if (count == buffer_size + buffer_size) {
                // we were able to find a single contiguous section
                // containing 2√A unique values, so this section
                // can be used to contain both of the internal
                // buffers we'll need
                buffer1 = Range<RandomAccessIterator>(
                    subrangeB.end - count, subrangeB.end - buffer_size);
                buffer2 = Range<RandomAccessIterator>(
                    subrangeB.end - buffer_size, subrangeB.end);
                return true;
            }
            else if (find == buffer_size + buffer_size) {
                // we found a buffer that contains at least √A
                // unique values, but did not contain the full 2√A
                // unique values, so we still need to find a second
                // separate buffer of at least √A unique values
                buffer1 = Range<RandomAccessIterator>(
                    subrangeB.end - count, subrangeB.end);
                find = buffer_size;
            }
            else if (block_size <= cache_size) {
                // we found the first and only internal buffer that
                // we need, so we're done!
                buffer1 = Range<RandomAccessIterator>(
                    subrangeB.end - count, subrangeB.end);
                return true;
            }
            else if (find_separately) {
                // found one buffer, but now find the other one
                buffer1 = Range<RandomAccessIterator>(
                    subrangeB.end - count, subrangeB.end);
                find_separately = false;
            }
            else {
                // buffer2 will be pulled out from a 'B' subarray,
                // so if the first buffer was pulled out from the
                // corresponding 'A' subarray, we need to adjust
                // the end point for that A subarray so it knows to
                // stop redistributing its values before reaching
                // buffer2
                if (pull_itrs[0].range.start == subrangeA.start) {
                    pull_itrs[0].range.end -= pull_itrs[1].count;
                }

                // we found a second buffer in a 'B' subarray
                // containing √A unique values, so we're done!
                buffer2 = Range<RandomAccessIterator>(
                    subrangeB.end - count, subrangeB.end);
                return true;
            }
        }
        else if (pull_index == 0 && count > buffer1.length()) {
            // keep track of the largest buffer we were able to
            // find
            buffer1 = Range<RandomAccessIterator>(subrangeB.end - count,
                                                  subrangeB.end);
            pull(subrangeB.end, subrangeA.start, subrangeB.end);
        }

        return false;
    }

    constexpr auto initialize_buffers(std::size_t chunk_length,
                                      auto const& compare)
    {
        // pull out the two ranges so we can use them as internal
        // buffers
        for (pull_index = 0; pull_index < 2; ++pull_index) {
            std::size_t length = pull_itrs[pull_index].count;

            if (pull_itrs[pull_index].to < pull_itrs[pull_index].from) {
                // we're pulling the values out to the left, which
                // means the start of an A subarray
                buffer_index = pull_itrs[pull_index].from;
                for (count = 1; count < length; ++count) {
                    buffer_index = FindFirstBackward(
                        pull_itrs[pull_index].to,
                        pull_itrs[pull_index].from - (count - 1),
                        *(buffer_index - 1),
                        compare,
                        length - count);
                    Range<RandomAccessIterator> left(
                        buffer_index + 1, pull_itrs[pull_index].from + 1);
                    std::rotate(left.start, left.end - count, left.end);
                    pull_itrs[pull_index].from = buffer_index + count;
                }
            }
            else if (pull_itrs[pull_index].to >
                     pull_itrs[pull_index].from) {
                // we're pulling values out to the right, which means
                // the end of a B subarray
                buffer_index = pull_itrs[pull_index].from + 1;
                for (count = 1; count < length; ++count) {
                    buffer_index =
                        FindLastForward(buffer_index,
                                        pull_itrs[pull_index].to,
                                        *buffer_index,
                                        compare,
                                        length - count);
                    Range<RandomAccessIterator> right(
                        pull_itrs[pull_index].from, buffer_index - 1);
                    std::rotate(
                        right.start, right.start + count, right.end);
                    pull_itrs[pull_index].from = buffer_index - count - 1;
                }
            }
        }

        // adjust block_size and buffer_size based on the values we
        // were able to pull out
        buffer_size = buffer1.length();
        block_size = chunk_length / buffer_size + 1;

        // the first buffer NEEDS to be large enough to tag each of the
        // evenly sized A blocks, so this was originally here to test
        // the math for adjusting block_size above
        // assert((chunk_length + 1)/block_size <= buffer_size);
    }

    constexpr auto adjust_ranges(auto&& subrangeA, auto&& subrangeB)
    {
        // remove any parts of A or B that are being used by the
        // internal buffers
        RandomAccessIterator start = subrangeA.start;
        if (start == pull_itrs[0].range.start) {
            if (pull_itrs[0].from > pull_itrs[0].to) {
                subrangeA.start += pull_itrs[0].count;

                // if the internal buffer takes up the entire A or
                // B subarray, then there's nothing to merge this
                // only happens for very small subarrays, like √4 =
                // 2, 2 * (2 internal buffers) = 4, which also only
                // happens when ranges::size(cache) is small or 0
                // since it'd otherwise use MergeExternal
                if (subrangeA.length() == 0) {
                    return true;
                }
            }
            else if (pull_itrs[0].from < pull_itrs[0].to) {
                subrangeB.end -= pull_itrs[0].count;
                if (subrangeB.length() == 0) {
                    return true;
                }
            }
        }
        if (start == pull_itrs[1].range.start) {
            if (pull_itrs[1].from > pull_itrs[1].to) {
                subrangeA.start += pull_itrs[1].count;
                if (subrangeA.length() == 0) {
                    return true;
                }
            }
            else if (pull_itrs[1].from < pull_itrs[1].to) {
                subrangeB.end -= pull_itrs[1].count;
                if (subrangeB.length() == 0) {
                    return true;
                }
            }
        }

        return false;
    }
};

template <ranges::random_access_range RandomAccessRange>
InternalBuffers(RandomAccessRange&, size_t, size_t)
    -> InternalBuffers<ranges::iterator_t<RandomAccessRange>>;

template <class T>
constexpr auto make_cache(std::size_t range_size,
                          std::optional<size_t> const& cache_size_opt = {})
{
    auto cache_size = cache_size_opt.value_or((range_size + 1) / 2);
    std::vector<T> cache;
    // good choices for the cache size are:
    // (size + 1)/2 – turns into a full-speed standard merge sort since
    // everything fits into the cache
    try {
        cache.resize(cache_size);
    }
    catch (std::bad_alloc&) {
        // sqrt((size + 1)/2) + 1 – this will be the size of the A
        // blocks at the largest level of merges, so a buffer of this
        // size would allow it to skip using internal or in-place
        // merges for anything
        try {
            cache_size = size_t(std::sqrt(cache_size) + 1);
            cache.resize(cache_size);
        }
        catch (std::bad_alloc&) {
            // 512 – chosen from careful testing as a good balance
            // between fixed-size memory use and run time
            if (cache_size > 512) {
                cache_size = 512;
                try {
                    cache.resize(cache_size);
                }
                catch (std::bad_alloc&) {
                }
            }
        }
    }
    return cache;
}

template <class RNG, class CMP = std::less<>>
constexpr auto sort_4(RNG& rng, CMP const& compare = {})
{
    using std::swap;
    if (ranges::size(rng) == 3) {
        // hard-coded insertion sort
        if (compare(rng[1], rng[0])) {
            swap(rng[0], rng[1]);
        }
        if (compare(rng[2], rng[1])) {
            swap(rng[2], rng[1]);
            if (compare(rng[1], rng[0])) {
                swap(rng[0], rng[1]);
            }
        }
    }
    else if (ranges::size(rng) == 2) {
        // swap the items if they're out of order
        if (compare(rng[1], rng[0])) {
            swap(rng[0], rng[1]);
        }
    }
}

constexpr auto initial_sort(auto& rng, auto& iterator, auto const& compare)
{
    while (!iterator.finished()) {
        std::array<size_t, 8> order = {0, 1, 2, 3, 4, 5, 6, 7};

        auto next_range = iterator.nextRange(rng);

        auto do_swap = [&](size_t x, size_t y) {
            using std::swap;
            if (compare(next_range.start[y], next_range.start[x]) ||
                (order.at(x) > order.at(y) &&
                 !compare(next_range.start[x], next_range.start[y]))) {
                std::iter_swap(next_range.start + x, next_range.start + y);
                swap(order.at(x), order.at(y));
            }
        };

        if (next_range.length() == 8) {
            do_swap(0, 1);
            do_swap(2, 3);
            do_swap(4, 5);
            do_swap(6, 7);
            do_swap(0, 2);
            do_swap(1, 3);
            do_swap(4, 6);
            do_swap(5, 7);
            do_swap(1, 2);
            do_swap(5, 6);
            do_swap(0, 4);
            do_swap(3, 7);
            do_swap(1, 5);
            do_swap(2, 6);
            do_swap(1, 4);
            do_swap(3, 6);
            do_swap(2, 4);
            do_swap(3, 5);
            do_swap(3, 4);
        }
        else if (next_range.length() == 7) {
            do_swap(1, 2);
            do_swap(3, 4);
            do_swap(5, 6);
            do_swap(0, 2);
            do_swap(3, 5);
            do_swap(4, 6);
            do_swap(0, 1);
            do_swap(4, 5);
            do_swap(2, 6);
            do_swap(0, 4);
            do_swap(1, 5);
            do_swap(0, 3);
            do_swap(2, 5);
            do_swap(1, 3);
            do_swap(2, 4);
            do_swap(2, 3);
        }
        else if (next_range.length() == 6) {
            do_swap(1, 2);
            do_swap(4, 5);
            do_swap(0, 2);
            do_swap(3, 5);
            do_swap(0, 1);
            do_swap(3, 4);
            do_swap(2, 5);
            do_swap(0, 3);
            do_swap(1, 4);
            do_swap(2, 4);
            do_swap(1, 3);
            do_swap(2, 3);
        }
        else if (next_range.length() == 5) {
            do_swap(0, 1);
            do_swap(3, 4);
            do_swap(2, 4);
            do_swap(2, 3);
            do_swap(1, 4);
            do_swap(0, 3);
            do_swap(0, 2);
            do_swap(1, 3);
            do_swap(1, 2);
        }
        else if (next_range.length() == 4) {
            do_swap(0, 1);
            do_swap(2, 3);
            do_swap(0, 2);
            do_swap(1, 3);
            do_swap(1, 2);
        }
    }
}

// sort using cache
constexpr auto block_sort_cache(auto&& iterator,
                                auto&& cache,
                                auto&& range,
                                auto&& compare)
{
    using RangeType = std::remove_cvref_t<decltype(range)>;
    using RandomAccessIterator = ranges::iterator_t<RangeType>;
    using diff_t = ranges::range_difference_t<RangeType>;

    // if four subarrays fit into the cache, it's faster to merge
    // both pairs of subarrays into the cache, then merge the two
    // merged subarrays from the cache back into the original array
    if ((iterator.length() + 1) * 4 <= ranges::size(cache) &&
        iterator.length() * 4 <= ranges::size(range)) {
        iterator.begin();
        while (!iterator.finished()) {
            // merge A1 and B1 into the cache
            auto subrangeA1 = iterator.nextRange(range);
            auto subrangeB1 = iterator.nextRange(range);
            auto subrangeA2 = iterator.nextRange(range);
            auto subrangeB2 = iterator.nextRange(range);

            if (compare(*(subrangeB1.end - 1), *subrangeA1.start)) {
                // the two ranges are in reverse order, so copy
                // them in reverse order into the cache
                ranges::copy(subrangeA1.start,
                             subrangeA1.end,
                             ranges::begin(cache) +
                                 diff_t(subrangeB1.length()));
                ranges::copy(subrangeB1.start,
                             subrangeB1.end,
                             ranges::begin(cache));
            }
            else if (compare(*subrangeB1.start, *(subrangeA1.end - 1))) {
                // these two ranges weren't already in order, so
                // merge them into the cache
                std::merge(subrangeA1.start,
                           subrangeA1.end,
                           subrangeB1.start,
                           subrangeB1.end,
                           ranges::begin(cache),
                           compare);
            }
            else {
                // if A1, B1, A2, and B2 are all in order, skip
                // doing anything else
                if (!compare(*subrangeB2.start, *(subrangeA2.end - 1)) &&
                    !compare(*subrangeA2.start, *(subrangeB1.end - 1))) {
                    continue;
                }

                // copy A1 and B1 into the cache in the same order
                // at once
                ranges::copy(subrangeA1.start,
                             subrangeB1.end,
                             ranges::begin(cache));
            }
            subrangeA1 = Range<RandomAccessIterator>(subrangeA1.start,
                                                     subrangeB1.end);

            // merge A2 and B2 into the cache
            if (compare(*(subrangeB2.end - 1), *subrangeA2.start)) {
                // the two ranges are in reverse order, so copy
                // them in reverse order into the cache
                ranges::copy(subrangeA2.start,
                             subrangeA2.end,
                             ranges::begin(cache) + subrangeA1.length() +
                                 subrangeB2.length());
                ranges::copy(subrangeB2.start,
                             subrangeB2.end,
                             ranges::begin(cache) + subrangeA1.length());
            }
            else if (compare(*subrangeB2.start, *(subrangeA2.end - 1))) {
                // these two ranges weren't already in order, so
                // merge them into the cache
                std::merge(subrangeA2.start,
                           subrangeA2.end,
                           subrangeB2.start,
                           subrangeB2.end,
                           ranges::begin(cache) + subrangeA1.length(),
                           compare);
            }
            else {
                // copy A2 and B2 into the cache in the same order
                // at once
                ranges::copy(subrangeA2.start,
                             subrangeB2.end,
                             ranges::begin(cache) + subrangeA1.length());
            }
            subrangeA2 = Range<RandomAccessIterator>(subrangeA2.start,
                                                     subrangeB2.end);

            // merge A1 and A2 from the cache into the array
            auto subrangeA3 =
                Range(ranges::begin(cache),
                      ranges::begin(cache) + subrangeA1.length());
            auto subrangeB3 =
                Range(ranges::begin(cache) + subrangeA1.length(),
                      ranges::begin(cache) + subrangeA1.length() +
                          subrangeA2.length());

            if (compare(*(subrangeB3.end - 1), *subrangeA3.start)) {
                // the two ranges are in reverse order, so copy
                // them in reverse order into the array
                ranges::copy(subrangeA3.start,
                             subrangeA3.end,
                             subrangeA1.start + subrangeA2.length());
                ranges::copy(
                    subrangeB3.start, subrangeB3.end, subrangeA1.start);
            }
            else if (compare(*subrangeB3.start, *(subrangeA3.end - 1))) {
                // these two ranges weren't already in order, so
                // merge them back into the array
                std::merge(subrangeA3.start,
                           subrangeA3.end,
                           subrangeB3.start,
                           subrangeB3.end,
                           subrangeA1.start,
                           compare);
            }
            else {
                // copy A3 and B3 into the array in the same order
                // at once
                ranges::copy(
                    subrangeA3.start, subrangeB3.end, subrangeA1.start);
            }
        }

        // we merged two levels at the same time, so we're done
        // with this level already (iterator.nextLevel() is called
        // again at the bottom of this outer merge loop)
        iterator.nextLevel();
    }
    else {
        iterator.begin();
        while (!iterator.finished()) {
            auto subrangeA = iterator.nextRange(range);
            auto subrangeB = iterator.nextRange(range);

            if (compare(*(subrangeB.end - 1), *subrangeA.start)) {
                // the two ranges are in reverse order, so a simple
                // rotation should fix it
                std::rotate(subrangeA.start, subrangeA.end, subrangeB.end);
            }
            else if (compare(*subrangeB.start, *(subrangeA.end - 1))) {
                // these two ranges weren't already in order, so
                // we'll need to merge them!
                ranges::copy(
                    subrangeA.start, subrangeA.end, ranges::begin(cache));
                MergeExternal(subrangeA.start,
                              subrangeA.end,
                              subrangeB.start,
                              subrangeB.end,
                              ranges::begin(cache),
                              compare);
            }
        }
    }
}

constexpr auto block_sort_internal(auto&& iterator,
                                   auto&& cache,
                                   auto&& range,
                                   auto&& compare)
{
    using RandomAccessIterator =
        ranges::iterator_t<std::remove_cvref_t<decltype(range)>>;
    // this is where the in-place merge logic starts!
    // 1. pull out two internal buffers each containing √A unique
    // values
    //     1a. adjust block_size and buffer_size if we couldn't
    //     find enough unique values
    // 2. loop over the A and B subarrays within this level of the
    // merge sort
    //     3. break A and B into blocks of size 'block_size'
    //     4. "tag" each of the A blocks with values from the first
    //     internal buffer
    //     5. roll the A blocks through the B blocks and
    //     drop/rotate them where they belong
    //     6. merge each A block with any B values that follow,
    //     using the cache or the second internal buffer
    // 7. sort the second internal buffer if it exists
    // 8. redistribute the two internal buffers back into the array

    // as an optimization, we really only need to pull out the
    // internal buffers once for each level of merges after that we
    // can reuse the same buffers over and over, then redistribute
    // it when we're finished with this level

    InternalBuffers buffers{range, iterator.length(), ranges::size(cache)};

    // we need to find either a single contiguous space containing
    // 2√A unique values (which will be split up into two buffers
    // of size √A each), or we need to find one buffer of < 2√A
    // unique values, and a second buffer of √A unique values, OR
    // if we couldn't find that many unique values, we need the
    // largest possible buffer we can get

    // in the case where it couldn't find a single buffer of at
    // least √A unique values, all of the Merge steps must be
    // replaced by a different merge algorithm (MergeInPlace)

    iterator.begin();
    while (!iterator.finished()) {
        auto subrangeA = iterator.nextRange(range);
        auto subrangeB = iterator.nextRange(range);

        if (buffers.unique(
                subrangeA, subrangeB, ranges::size(cache), compare)) {
            break;
        }
    }

    buffers.initialize_buffers(iterator.length(), compare);

    // now that the two internal buffers have been created, it's
    // time to merge each A+B combination at this level of the
    // merge sort!
    iterator.begin();
    while (!iterator.finished()) {
        auto subrangeA = iterator.nextRange(range);
        auto subrangeB = iterator.nextRange(range);

        if (buffers.adjust_ranges(subrangeA, subrangeB)) {
            continue;
        }

        if (compare(*(subrangeB.end - 1), *subrangeA.start)) {
            // the two ranges are in reverse order, so a simple
            // rotation should fix it
            std::rotate(subrangeA.start, subrangeA.end, subrangeB.end);
        }
        else if (compare(*subrangeA.end, *(subrangeA.end - 1))) {
            // these two ranges weren't already in order, so we'll
            // need to merge them!

            // break the remainder of A into blocks. firstA is the
            // uneven-sized first A block
            Range<RandomAccessIterator> blockA(subrangeA);
            Range<RandomAccessIterator> firstA(
                subrangeA.start,
                subrangeA.start + blockA.length() % buffers.block_size);

            // swap the first value of each A block with the values
            // in buffer1
            for (RandomAccessIterator indexA = buffers.buffer1.start,
                                      index = firstA.end;
                 index < blockA.end;
                 ++indexA, index += buffers.block_size) {
                std::iter_swap(indexA, index);
            }

            // start rolling the A blocks through the B blocks!
            // when we leave an A block behind we'll need to merge
            // the previous A block with any B blocks that follow
            // it, so track that information as well
            Range<RandomAccessIterator> lastA(firstA);
            Range<RandomAccessIterator> lastB(ranges::begin(range),
                                              ranges::begin(range));
            Range<RandomAccessIterator> blockB(
                subrangeB.start,
                subrangeB.start +
                    std::min(buffers.block_size, subrangeB.length()));
            blockA.start += firstA.length();
            RandomAccessIterator indexA = buffers.buffer1.start;

            // if the first unevenly sized A block fits into the
            // cache, copy it there for when we go to Merge it
            // otherwise, if the second buffer is available, block
            // swap the contents into that
            if (lastA.length() <= ranges::size(cache)) {
                ranges::copy(lastA.start, lastA.end, ranges::begin(cache));
            }
            else if (buffers.buffer2.length() > 0) {
                std::swap_ranges(
                    lastA.start, lastA.end, buffers.buffer2.start);
            }

            if (blockA.length() > 0) {
                while (true) {
                    // if there's a previous B block and the first
                    // value of the minimum A block is <= the last
                    // value of the previous B block, then drop
                    // that minimum A block behind. or if there are
                    // no B blocks left then keep dropping the
                    // remaining A blocks.
                    if ((lastB.length() > 0 &&
                         !compare(*(lastB.end - 1), *indexA)) ||
                        blockB.length() == 0) {
                        // figure out where to split the previous B
                        // block, and rotate it at the split
                        RandomAccessIterator B_split = std::lower_bound(
                            lastB.start, lastB.end, *indexA, compare);
                        std::size_t B_remaining =
                            std::distance(B_split, lastB.end);

                        // swap the minimum A block to the
                        // beginning of the rolling A blocks
                        RandomAccessIterator minA = blockA.start;
                        for (RandomAccessIterator findA =
                                 minA + buffers.block_size;
                             findA < blockA.end;
                             findA += buffers.block_size) {
                            if (compare(*findA, *minA)) {
                                minA = findA;
                            }
                        }
                        std::swap_ranges(blockA.start,
                                         blockA.start + buffers.block_size,
                                         minA);

                        // swap the first item of the previous A
                        // block back with its original value,
                        // which is stored in buffer1
                        std::iter_swap(blockA.start, indexA);
                        ++indexA;

                        // locally merge the previous A block with
                        // the B values that follow it if lastA
                        // fits into the external cache we'll use
                        // that (with MergeExternal), or if the
                        // second internal buffer exists we'll use
                        // that (with MergeInternal), or failing
                        // that we'll use a strictly in-place merge
                        // algorithm (MergeInPlace)
                        if (lastA.length() <= ranges::size(cache)) {
                            MergeExternal(lastA.start,
                                          lastA.end,
                                          lastA.end,
                                          B_split,
                                          ranges::begin(cache),
                                          compare);
                        }
                        else if (buffers.buffer2.length() > 0) {
                            MergeInternal(lastA.start,
                                          lastA.end,
                                          lastA.end,
                                          B_split,
                                          buffers.buffer2.start,
                                          compare);
                        }
                        else {
                            MergeInPlace(lastA.start,
                                         lastA.end,
                                         lastA.end,
                                         B_split,
                                         compare);
                        }

                        if (buffers.buffer2.length() > 0 ||
                            buffers.block_size <= ranges::size(cache)) {
                            // copy the previous A block into the
                            // cache or buffer2, since that's where
                            // we need it to be when we go to merge
                            // it anyway
                            if (buffers.block_size <=
                                ranges::size(cache)) {
                                ranges::copy(blockA.start,
                                             blockA.start +
                                                 buffers.block_size,
                                             ranges::begin(cache));
                            }
                            else {
                                std::swap_ranges(blockA.start,
                                                 blockA.start +
                                                     buffers.block_size,
                                                 buffers.buffer2.start);
                            }

                            // this is equivalent to rotating, but
                            // faster the area normally taken up by
                            // the A block is either the contents
                            // of buffer2, or data we don't need
                            // anymore since we memcopied it either
                            // way we don't need to retain the
                            // order of those items, so instead of
                            // rotating we can just block swap B to
                            // where it belongs
                            std::swap_ranges(B_split,
                                             B_split + B_remaining,
                                             blockA.start +
                                                 buffers.block_size -
                                                 B_remaining);
                        }
                        else {
                            // we are unable to use the 'buffer2'
                            // trick to speed up the rotation
                            // operation since buffer2 doesn't
                            // exist, so perform a normal rotation
                            std::rotate(B_split,
                                        blockA.start,
                                        blockA.start + buffers.block_size);
                        }

                        // update the range for the remaining A
                        // blocks, and the range remaining from the
                        // B block after it was split
                        lastA = Range<RandomAccessIterator>(
                            blockA.start - B_remaining,
                            blockA.start - B_remaining +
                                buffers.block_size);
                        lastB = Range<RandomAccessIterator>(
                            lastA.end, lastA.end + B_remaining);

                        // if there are no more A blocks remaining,
                        // this step is finished!
                        blockA.start += buffers.block_size;
                        if (blockA.length() == 0) {
                            break;
                        }
                    }
                    else if (blockB.length() < buffers.block_size) {
                        // move the last B block, which is unevenly
                        // sized, to before the remaining A blocks,
                        // by using a rotation
                        std::rotate(
                            blockA.start, blockB.start, blockB.end);

                        lastB = Range<RandomAccessIterator>(
                            blockA.start, blockA.start + blockB.length());
                        blockA.start += blockB.length();
                        blockA.end += blockB.length();
                        blockB.end = blockB.start;
                    }
                    else {
                        // roll the leftmost A block to the end by
                        // swapping it with the next B block
                        std::swap_ranges(blockA.start,
                                         blockA.start + buffers.block_size,
                                         blockB.start);
                        lastB = Range<RandomAccessIterator>(
                            blockA.start,
                            blockA.start + buffers.block_size);

                        blockA.start += buffers.block_size;
                        blockA.end += buffers.block_size;
                        blockB.start += buffers.block_size;

                        if (blockB.end >
                            subrangeB.end - buffers.block_size) {
                            blockB.end = subrangeB.end;
                        }
                        else {
                            blockB.end += buffers.block_size;
                        }
                    }
                }
            }

            // merge the last A block with the remaining B values
            if (lastA.length() <= ranges::size(cache)) {
                MergeExternal(lastA.start,
                              lastA.end,
                              lastA.end,
                              subrangeB.end,
                              ranges::begin(cache),
                              compare);
            }
            else if (buffers.buffer2.length() > 0) {
                MergeInternal(lastA.start,
                              lastA.end,
                              lastA.end,
                              subrangeB.end,
                              buffers.buffer2.start,
                              compare);
            }
            else {
                MergeInPlace(lastA.start,
                             lastA.end,
                             lastA.end,
                             subrangeB.end,
                             compare);
            }
        }
    }

    // when we're finished with this merge step we should have the
    // one or two internal buffers left over, where the second
    // buffer is all jumbled up insertion sort the second buffer,
    // then redistribute the buffers back into the array using the
    // opposite process used for creating the buffer

    // while an unstable sort like std::sort could be applied here,
    // in benchmarks it was consistently slightly slower than a
    // simple insertion sort, even for tens of millions of items.
    // this may be because insertion sort is quite fast when the
    // data is already somewhat sorted, like it is here
    InsertionSort(buffers.buffer2.start, buffers.buffer2.end, compare);

    for (buffers.pull_index = 0; buffers.pull_index < 2;
         ++buffers.pull_index) {
        std::size_t unique =
            buffers.pull_itrs[buffers.pull_index].count * 2;
        if (buffers.pull_itrs[buffers.pull_index].from >
            buffers.pull_itrs[buffers.pull_index].to) {
            // the values were pulled out to the left, so
            // redistribute them back to the right
            Range<RandomAccessIterator> buffer(
                buffers.pull_itrs[buffers.pull_index].range.start,
                buffers.pull_itrs[buffers.pull_index].range.start +
                    buffers.pull_itrs[buffers.pull_index].count);
            while (buffer.length() > 0) {
                buffers.buffer_index = FindFirstForward(
                    buffer.end,
                    buffers.pull_itrs[buffers.pull_index].range.end,
                    *buffer.start,
                    compare,
                    unique);
                std::size_t amount = buffers.buffer_index - buffer.end;
                std::rotate(
                    buffer.start, buffer.end, buffers.buffer_index);
                buffer.start += (amount + 1);
                buffer.end += amount;
                unique -= 2;
            }
        }
        else if (buffers.pull_itrs[buffers.pull_index].from <
                 buffers.pull_itrs[buffers.pull_index].to) {
            // the values were pulled out to the right, so
            // redistribute them back to the left
            Range<RandomAccessIterator> buffer(
                buffers.pull_itrs[buffers.pull_index].range.end -
                    buffers.pull_itrs[buffers.pull_index].count,
                buffers.pull_itrs[buffers.pull_index].range.end);
            while (buffer.length() > 0) {
                buffers.buffer_index = FindLastBackward(
                    buffers.pull_itrs[buffers.pull_index].range.start,
                    buffer.start,
                    *(buffer.end - 1),
                    compare,
                    unique);
                std::size_t amount = buffer.start - buffers.buffer_index;
                std::rotate(buffers.buffer_index,
                            buffers.buffer_index + amount,
                            buffer.end);
                buffer.start -= amount;
                buffer.end -= (amount + 1);
                unique -= 2;
            }
        }
    }
}

// bottom-up merge sort combined with an in-place merge algorithm for O(1)
// memory use
template <ranges::contiguous_range RANGE,
          typename Comparison = std::less<>>
constexpr auto block_sort(RANGE range,
                          Comparison const& compare = {},
                          std::optional<size_t> const& cache_size_opt = {})
    -> RANGE
{
    // map first and last to a C-style array, so we don't have to change
    // the rest of the code (bit of a nasty hack, but it's good enough for
    // now...)

    using T = ranges::range_value_t<RANGE>;

    // if the array is of size 0, 1, 2, or 3, just sort them like so:
    if (ranges::size(range) < 4) {
        sort_4(range, compare);
        return range;
    }

    // sort groups of 4-8 items at a time using an unstable sorting
    // network, but keep track of the original item orders to force it to
    // be stable http://pages.ripco.net/~jgamble/nw.html
    Iterator iterator(ranges::size(range), 4);

    initial_sort(range, iterator, compare);
    if (ranges::size(range) < 8) {
        return range;
    }

    // use a small cache to speed up some of the operations
    auto cache = make_cache<T>(ranges::size(range), cache_size_opt);

    // then merge sort the higher levels, which can be 8-15, 16-31, 32-63,
    // 64-127, etc.
    while (true) {
        // if every A and B block will fit into the cache, use a special
        // branch specifically for merging with the cache (we use < rather
        // than <= since the block size might be one more than
        // iterator.length())
        if (iterator.length() < ranges::size(cache)) {
            block_sort_cache(iterator, cache, range, compare);
        }
        else {
            block_sort_internal(iterator, cache, range, compare);
        }

        // double the size of each A and B subarray that will be merged in
        // the next level
        if (!iterator.nextLevel()) {
            break;
        }
    }

    return range;
}

template <class COMPARE>
struct block_sort_adapter
{
    COMPARE compare;
    std::optional<size_t> cache_size;

    friend constexpr auto operator|(ranges::contiguous_range auto&& range,
                                    block_sort_adapter const& adapter)
    {
        return block_sort(std::forward<decltype(range)>(range),
                          adapter.compare,
                          adapter.cache_size);
    }

    constexpr auto operator()(ranges::contiguous_range auto&& range)
    {
        return block_sort(
            std::forward<decltype(range)>(range), compare, cache_size);
    }
};

} // namespace _block_sort

constexpr struct
{
    static constexpr auto operator()(
        auto const& compare,
        std::optional<size_t> const& cache_size = std::nullopt)
    {
        return _block_sort::block_sort_adapter{
            std::forward<decltype(compare)>(compare), cache_size};
    }

    static constexpr auto operator()(
        ranges::contiguous_range auto&& range,
        auto const& compare = std::less<>{},
        std::optional<size_t> const& cache_size = std::nullopt)
    {
        return _block_sort::block_sort(
            std::forward<decltype(range)>(range),
            std::forward<decltype(compare)>(compare),
            cache_size);
    }

    static constexpr auto operator()()
    {
        return _block_sort::block_sort_adapter{std::less<>{},
                                               std::nullopt};
    }

} block_sort;

constexpr auto block_sort_ascending = block_sort(std::less<>{});
constexpr auto block_sort_decending = block_sort(std::greater<>{});

} // namespace algo
