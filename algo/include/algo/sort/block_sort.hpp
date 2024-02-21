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
#include <bit>
#include <cassert>
#include <cmath>
#include <iterator>

#include <range/v3/all.hpp>

#include "ordering.hpp"

#include <algo/prelude.hpp>

namespace algo
{
namespace _block_sort
{

constexpr auto adjust_begin(auto& range, auto diff)
{
    using difference_type =
        ranges::range_difference_t<std::remove_cvref_t<decltype(range)>>;

    return ranges::subrange(ranges::begin(range) + difference_type(diff),
                            ranges::end(range));
}

constexpr auto adjust_end(auto& range, auto diff)
{
    using difference_type =
        ranges::range_difference_t<std::remove_cvref_t<decltype(range)>>;

    return ranges::subrange(ranges::begin(range),
                            ranges::end(range) + difference_type(diff));
}

constexpr auto shift_range(auto& range, auto diff)
{
    using difference_type =
        ranges::range_difference_t<std::remove_cvref_t<decltype(range)>>;

    return ranges::subrange(ranges::begin(range) + difference_type(diff),
                            ranges::end(range) + difference_type(diff));
}
// combine a linear search with a binary search to reduce the number of
// comparisons in situations where have some idea as to how many unique
// values there are and where the next value might be
template <typename RandomAccessIterator, typename T, typename Comparison>
RandomAccessIterator FindFirstForward(RandomAccessIterator first,
                                      RandomAccessIterator last,
                                      const T& value,
                                      Comparison compare,
                                      auto unique)
{
    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    auto size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    auto skip =
        std::max(size / difference_type(unique), difference_type(1));

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
                                     auto unique)
{
    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    auto size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    auto skip =
        std::max(size / difference_type(unique), difference_type(1));

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
                                       auto unique)
{
    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    auto size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    auto skip =
        std::max(size / difference_type(unique), difference_type(1));

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
                                      auto unique)
{
    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    auto size = std::distance(first, last);
    if (size == 0) {
        return first;
    }

    auto skip =
        std::max(size / difference_type(unique), difference_type(1));

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
    using value_type =
        std::iterator_traits<BidirectionalIterator>::value_type;

    if (first == last) {
        return;
    }

    for (BidirectionalIterator cur = first + 1; cur != last; ++cur) {
        BidirectionalIterator sift = cur;
        BidirectionalIterator sift_1 = cur - 1;

        // Compare first so we can avoid 2 moves for
        // an element already positioned correctly.
        if (compare(*sift, *sift_1)) {
            value_type tmp = *sift;
            do { // NOLINT
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
    using difference_type =
        std::iterator_traits<RandomAccessIterator1>::difference_type;

    // A fits into the cache, so use that instead of the internal buffer
    RandomAccessIterator2 A_index = cache;
    RandomAccessIterator2 A_last =
        cache + difference_type(std::distance(first1, last1));
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
    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    RandomAccessIterator A_index = buffer;
    RandomAccessIterator B_index = first2;
    RandomAccessIterator A_last =
        buffer + difference_type(std::distance(first1, last1));
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

    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    while (true) {
        // find the first place in B where the first item in A needs to be
        // inserted
        RandomAccessIterator mid =
            std::lower_bound(first2, last2, *first1, compare);

        // rotate A into place
        difference_type amount = mid - last1;
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
template <class RangeType>
class Iterator
{
    using difference_type = ranges::range_difference_t<RangeType>;

    difference_type size, power_of_two;
    difference_type decimal{}, numerator{}, denominator;
    difference_type decimal_step, numerator_step;

public:
    Iterator(difference_type size, difference_type min_level)
        : size(size)
        , power_of_two(difference_type(std::bit_floor(size_t(size))))
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
    auto nextRange(RANGE& range)
        -> ranges::subrange<ranges::iterator_t<RANGE>>
    {
        auto start = decimal;

        decimal += decimal_step;
        numerator += numerator_step;
        if (numerator >= denominator) {
            numerator -= denominator;
            ++decimal;
        }

        return ranges::subrange(ranges::begin(range) + start,
                                ranges::begin(range) + decimal);
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

    [[nodiscard]] auto length() const
    {
        return decimal_step;
    }
};

template <class RandomAccessIterator>
struct InternalBuffers
{
    using difference_type =
        std::iterator_traits<RandomAccessIterator>::difference_type;

    // as an optimization, we really only need to pull out the
    // internal buffers once for each level of merges after that we
    // can reuse the same buffers over and over, then redistribute
    // it when we're finished with this level
    difference_type block_size;
    difference_type buffer_size;
    difference_type find;
    difference_type count = 0;
    std::size_t pull_index = 0;
    ranges::subrange<RandomAccessIterator> buffer1{};
    ranges::subrange<RandomAccessIterator> buffer2{};
    RandomAccessIterator buffer_index;
    RandomAccessIterator last;
    struct PullItr
    {
        RandomAccessIterator from{}, to{};
        difference_type count{};
        ranges::subrange<RandomAccessIterator> range;

        constexpr explicit PullItr(
            ranges::random_access_range auto&& range)
            : range{range}
        {
        }
    };
    std::array<PullItr, 2> pull_itrs{};
    bool find_separately = false;

    constexpr explicit InternalBuffers(
        auto&& iterator,
        ranges::random_access_range auto&& range,
        auto&& cache,
        auto&& compare)
        : block_size{difference_type(std::sqrt(iterator.length()))}
        , buffer_size{difference_type(iterator.length()) / block_size + 1}
        , find{buffer_size + buffer_size}
        , buffer1{range}
        , buffer2{range}
        , pull_itrs{{PullItr(range), PullItr(range)}}
    {
        // find two internal buffers of size 'buffer_size' each
        // let's try finding both buffers at the same time from a
        // single A or B subarray
        if (block_size <= difference_type(ranges::size(cache))) {
            // if every A block fits into the cache then we won't need
            // the second internal buffer, so we really only need to
            // find 'buffer_size' unique values
            find = buffer_size;
        }
        else if (find > difference_type(iterator.length())) {
            // we can't fit both buffers into the same A or B subarray,
            // so find two buffers separately
            find = buffer_size;
            find_separately = true;
        }

        initialize(iterator, range, cache, compare);
    }

    constexpr void clean_up(auto&& compare)
    {
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
        InsertionSort(
            ranges::begin(buffer2), ranges::end(buffer2), compare);

        for (pull_index = 0; pull_index < 2; ++pull_index) {
            auto unique = pull_itrs[pull_index].count * 2;
            if (pull_itrs[pull_index].from > pull_itrs[pull_index].to) {
                // the values were pulled out to the left, so
                // redistribute them back to the right
                ranges::subrange buffer(
                    ranges::begin(pull_itrs[pull_index].range),
                    ranges::begin(pull_itrs[pull_index].range) +
                        pull_itrs[pull_index].count);
                while (ranges::size(buffer) > 0) {
                    buffer_index = FindFirstForward(
                        ranges::end(buffer),
                        ranges::end(pull_itrs[pull_index].range),
                        *ranges::begin(buffer),
                        compare,
                        unique);
                    difference_type amount =
                        buffer_index - ranges::end(buffer);
                    std::rotate(ranges::begin(buffer),
                                ranges::end(buffer),
                                buffer_index);
                    buffer = ranges::subrange(
                        ranges::begin(buffer) + (amount + 1),
                        ranges::end(buffer) + amount);
                    unique -= 2;
                }
            }
            else if (pull_itrs[pull_index].from <
                     pull_itrs[pull_index].to) {
                // the values were pulled out to the right, so
                // redistribute them back to the left
                ranges::subrange buffer(
                    ranges::end(pull_itrs[pull_index].range) -
                        pull_itrs[pull_index].count,
                    ranges::end(pull_itrs[pull_index].range));
                while (ranges::size(buffer) > 0) {
                    buffer_index = FindLastBackward(
                        ranges::begin(pull_itrs[pull_index].range),
                        ranges::begin(buffer),
                        *(ranges::end(buffer) - 1),
                        compare,
                        unique);
                    difference_type amount =
                        ranges::begin(buffer) - buffer_index;
                    std::rotate(buffer_index,
                                buffer_index + amount,
                                ranges::end(buffer));
                    buffer = ranges::subrange(
                        ranges::begin(buffer) - amount,
                        ranges::end(buffer) - (amount + 1));
                    unique -= 2;
                }
            }
        }
    }

    // just store information about where the values will be
    // pulled from and to, as well as how many values there
    // are, to create the two internal buffers
    constexpr auto pull(auto&& destination, auto&& start, auto&& end)
    {
        pull_itrs[pull_index].range =
            ranges::subrange(std::forward<decltype(start)>(start),
                             std::forward<decltype(end)>(end));
        pull_itrs[pull_index].count = count;
        pull_itrs[pull_index].from = buffer_index;
        pull_itrs[pull_index].to =
            std::forward<decltype(destination)>(destination);
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
        for (last = ranges::begin(subrangeA), count = 1; count < find;
             last = buffer_index, ++count) {
            buffer_index = FindLastForward(last + 1,
                                           ranges::end(subrangeA),
                                           *last,
                                           compare,
                                           find - count);
            if (buffer_index == ranges::end(subrangeA)) {
                break;
            }
            assert(buffer_index < ranges::end(subrangeA));
        }
        buffer_index = last;

        if (count >= buffer_size) {
            // keep track of the range within the array where we'll
            // need to "pull out" these values to create the
            // internal buffer
            pull(ranges::begin(subrangeA),
                 ranges::begin(subrangeA),
                 ranges::end(subrangeB));
            pull_index = 1;

            if (count == buffer_size + buffer_size) {
                // we were able to find a single contiguous section
                // containing 2√A unique values, so this section
                // can be used to contain both of the internal
                // buffers we'll need
                buffer1 =
                    ranges::subrange(ranges::begin(subrangeA),
                                     ranges::begin(subrangeA) +
                                         difference_type(buffer_size));
                buffer2 = ranges::subrange(
                    ranges::begin(subrangeA) +
                        difference_type(buffer_size),
                    ranges::begin(subrangeA) + difference_type(count));
                return true;
            }
            else if (find == buffer_size + buffer_size) {
                // we found a buffer that contains at least √A
                // unique values, but did not contain the full 2√A
                // unique values, so we still need to find a second
                // separate buffer of at least √A unique values
                buffer1 = ranges::subrange(ranges::begin(subrangeA),
                                           ranges::begin(subrangeA) +
                                               difference_type(count));
                find = buffer_size;
            }
            else if (block_size <= difference_type(cache_size)) {
                // we found the first and only internal buffer that
                // we need, so we're done!
                buffer1 = ranges::subrange(ranges::begin(subrangeA),
                                           ranges::begin(subrangeA) +
                                               difference_type(count));
                return true;
            }
            else if (find_separately) {
                // found one buffer, but now find the other one
                buffer1 = ranges::subrange(ranges::begin(subrangeA),
                                           ranges::begin(subrangeA) +
                                               difference_type(count));
                find_separately = false;
            }
            else {
                // we found a second buffer in an 'A' subarray
                // containing √A unique values, so we're done!
                buffer2 = ranges::subrange(ranges::begin(subrangeA),
                                           ranges::begin(subrangeA) +
                                               difference_type(count));
                return true;
            }
        }
        else if (pull_index == 0 &&
                 count > difference_type(ranges::size(buffer1))) {
            // keep track of the largest buffer we were able to
            // find
            buffer1 = ranges::subrange(ranges::begin(subrangeA),
                                       ranges::begin(subrangeA) +
                                           difference_type(count));
            pull(ranges::begin(subrangeA),
                 ranges::begin(subrangeA),
                 ranges::end(subrangeB));
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
        for (last = ranges::end(subrangeB) - 1, count = 1; count < find;
             last = buffer_index - 1, ++count) {
            buffer_index = FindFirstBackward(ranges::begin(subrangeB),
                                             last,
                                             *last,
                                             compare,
                                             find - count);
            if (buffer_index == ranges::begin(subrangeB)) {
                break;
            }
            assert(buffer_index > ranges::begin(subrangeB));
        }
        buffer_index = last;

        if (count >= buffer_size) {
            // keep track of the range within the array where we'll
            // need to "pull out" these values to create the
            // internal buffer
            pull(ranges::end(subrangeB),
                 ranges::begin(subrangeA),
                 ranges::end(subrangeB));
            pull_index = 1;

            if (count == buffer_size + buffer_size) {
                // we were able to find a single contiguous section
                // containing 2√A unique values, so this section
                // can be used to contain both of the internal
                // buffers we'll need
                buffer1 = ranges::subrange(
                    ranges::end(subrangeB) - difference_type(count),
                    ranges::end(subrangeB) - difference_type(buffer_size));
                buffer2 = ranges::subrange(
                    ranges::end(subrangeB) - difference_type(buffer_size),
                    ranges::end(subrangeB));
                return true;
            }
            else if (find == buffer_size + buffer_size) {
                // we found a buffer that contains at least √A
                // unique values, but did not contain the full 2√A
                // unique values, so we still need to find a second
                // separate buffer of at least √A unique values
                buffer1 = ranges::subrange(ranges::end(subrangeB) -
                                               difference_type(count),
                                           ranges::end(subrangeB));
                find = buffer_size;
            }
            else if (block_size <= difference_type(cache_size)) {
                // we found the first and only internal buffer that
                // we need, so we're done!
                buffer1 = ranges::subrange(ranges::end(subrangeB) -
                                               difference_type(count),
                                           ranges::end(subrangeB));
                return true;
            }
            else if (find_separately) {
                // found one buffer, but now find the other one
                buffer1 = ranges::subrange(ranges::end(subrangeB) -
                                               difference_type(count),
                                           ranges::end(subrangeB));
                find_separately = false;
            }
            else {
                // buffer2 will be pulled out from a 'B' subarray,
                // so if the first buffer was pulled out from the
                // corresponding 'A' subarray, we need to adjust
                // the end point for that A subarray so it knows to
                // stop redistributing its values before reaching
                // buffer2
                if (ranges::begin(pull_itrs[0].range) ==
                    ranges::begin(subrangeA)) {
                    pull_itrs[0].range =
                        adjust_end(pull_itrs[0].range,
                                   -difference_type(pull_itrs[1].count));
                }

                // we found a second buffer in a 'B' subarray
                // containing √A unique values, so we're done!
                buffer2 = ranges::subrange(ranges::end(subrangeB) -
                                               difference_type(count),
                                           ranges::end(subrangeB));
                return true;
            }
        }
        else if (pull_index == 0 &&
                 count > difference_type(ranges::size(buffer1))) {
            // keep track of the largest buffer we were able to
            // find
            buffer1 = ranges::subrange(ranges::end(subrangeB) -
                                           difference_type(count),
                                       ranges::end(subrangeB));
            pull(ranges::end(subrangeB),
                 ranges::begin(subrangeA),
                 ranges::end(subrangeB));
        }

        return false;
    }

    // we need to find either a single contiguous space containing
    // 2√A unique values (which will be split up into two buffers
    // of size √A each), or we need to find one buffer of < 2√A
    // unique values, and a second buffer of √A unique values, OR
    // if we couldn't find that many unique values, we need the
    // largest possible buffer we can get
    // in the case where it couldn't find a single buffer of at
    // least √A unique values, all of the Merge steps must be
    // replaced by a different merge algorithm (MergeInPlace)
    constexpr auto initialize(auto&& iterator,
                              auto&& range,
                              auto&& cache,
                              auto const& compare)
    {
        iterator.begin();
        while (!iterator.finished()) {
            auto subrangeA = iterator.nextRange(range);
            auto subrangeB = iterator.nextRange(range);

            if (unique(
                    subrangeA, subrangeB, ranges::size(cache), compare)) {
                break;
            }
        }

        // pull out the two ranges so we can use them as internal
        // buffers
        for (pull_index = 0; pull_index < 2; ++pull_index) {
            auto length = pull_itrs[pull_index].count;

            if (pull_itrs[pull_index].to < pull_itrs[pull_index].from) {
                // we're pulling the values out to the left, which
                // means the start of an A subarray
                buffer_index = pull_itrs[pull_index].from;
                for (count = 1; count < length; ++count) {
                    buffer_index =
                        FindFirstBackward(pull_itrs[pull_index].to,
                                          pull_itrs[pull_index].from -
                                              (difference_type(count) - 1),
                                          *(buffer_index - 1),
                                          compare,
                                          length - count);
                    auto left = ranges::subrange(
                        buffer_index + 1, pull_itrs[pull_index].from + 1);
                    ranges::rotate(
                        left, ranges::end(left) - difference_type(count));
                    pull_itrs[pull_index].from =
                        buffer_index + difference_type(count);
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
                    ranges::subrange right(pull_itrs[pull_index].from,
                                           buffer_index - 1);
                    ranges::rotate(right,
                                   ranges::begin(right) +
                                       difference_type(count));
                    pull_itrs[pull_index].from =
                        buffer_index - difference_type(count) - 1;
                }
            }
        }

        // adjust block_size and buffer_size based on the values we
        // were able to pull out
        buffer_size = difference_type(ranges::size(buffer1));
        block_size = iterator.length() / buffer_size + 1;

        // the first buffer NEEDS to be large enough to tag each of the
        // evenly sized A blocks, so this was originally here to test
        // the math for adjusting block_size above
        // assert((chunk_length + 1)/block_size <= buffer_size);
    }

    constexpr auto adjust_ranges(auto&& subrangeA, auto&& subrangeB)
    {
        // remove any parts of A or B that are being used by the
        // internal buffers
        RandomAccessIterator start = ranges::begin(subrangeA);
        if (start == ranges::begin(pull_itrs[0].range)) {
            if (pull_itrs[0].from > pull_itrs[0].to) {
                subrangeA = adjust_begin(subrangeA, pull_itrs[0].count);

                // if the internal buffer takes up the entire A or
                // B subarray, then there's nothing to merge this
                // only happens for very small subarrays, like √4 =
                // 2, 2 * (2 internal buffers) = 4, which also only
                // happens when ranges::size(cache) is small or 0
                // since it'd otherwise use MergeExternal
                if (difference_type(ranges::size(subrangeA)) == 0) {
                    return true;
                }
            }
            else if (pull_itrs[0].from < pull_itrs[0].to) {
                subrangeB = adjust_end(
                    subrangeB, -difference_type(pull_itrs[0].count));
                if (difference_type(ranges::size(subrangeB)) == 0) {
                    return true;
                }
            }
        }
        if (start == ranges::begin(pull_itrs[1].range)) {
            if (pull_itrs[1].from > pull_itrs[1].to) {
                subrangeA = adjust_begin(subrangeA, pull_itrs[1].count);
                if (difference_type(ranges::size(subrangeA)) == 0) {
                    return true;
                }
            }
            else if (pull_itrs[1].from < pull_itrs[1].to) {
                subrangeB = adjust_end(
                    subrangeB, -difference_type(pull_itrs[1].count));
                if (difference_type(ranges::size(subrangeB)) == 0) {
                    return true;
                }
            }
        }

        return false;
    }
};

template <ranges::random_access_range RandomAccessRange>
InternalBuffers(auto&&, RandomAccessRange&, auto&&, auto&&)
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
constexpr auto sort_four(RNG& rng, CMP const& compare = {})
{
    using difference_type = ranges::range_difference_t<RNG>;
    using std::swap;
    if (difference_type(ranges::size(rng)) == 3) {
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
    else if (difference_type(ranges::size(rng)) == 2) {
        // swap the items if they're out of order
        if (compare(rng[1], rng[0])) {
            swap(rng[0], rng[1]);
        }
    }
}

constexpr auto initial_sort(auto& rng, auto& iterator, auto const& compare)
{
    using difference_type =
        ranges::range_difference_t<std::remove_cvref_t<decltype(rng)>>;

    while (!iterator.finished()) {
        std::array<size_t, 8> order = {0, 1, 2, 3, 4, 5, 6, 7};

        auto next_range = iterator.nextRange(rng);

        auto do_swap = [&](size_t x, size_t y) {
            using std::swap;
            ASSUME(x < 8 && y < 8 && x >= 0 && y >= 0);
            if (compare(next_range[difference_type(y)],
                        next_range[difference_type(x)]) ||
                (order.at(x) > order.at(y) &&
                 !compare(next_range[difference_type(x)],
                          next_range[difference_type(y)]))) {
                swap(next_range[difference_type(x)],
                     next_range[difference_type(y)]);
                swap(order.at(x), order.at(y));
            }
        };

        if (ranges::size(next_range) == 8) {
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
        else if (ranges::size(next_range) == 7) {
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
        else if (ranges::size(next_range) == 6) {
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
        else if (ranges::size(next_range) == 5) {
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
        else if (ranges::size(next_range) == 4) {
            do_swap(0, 1);
            do_swap(2, 3);
            do_swap(0, 2);
            do_swap(1, 3);
            do_swap(1, 2);
        }
    }
}

// merge 2 chunks at a time using an in memory cache
constexpr auto merge_two_chunks(auto&& subrangeA,
                                auto&& subrangeB,
                                auto&& cache,
                                auto&& compare)
{

    if (compare(*(ranges::end(subrangeB) - 1),
                *ranges::begin(subrangeA))) {
        // the two ranges are in reverse order, so a simple
        // rotation should fix it
        std::rotate(ranges::begin(subrangeA),
                    ranges::end(subrangeA),
                    ranges::end(subrangeB));
    }
    else if (compare(*ranges::begin(subrangeB),
                     *(ranges::end(subrangeA) - 1))) {
        // these two ranges weren't already in order, so
        // we'll need to merge them!
        ranges::copy(subrangeA, ranges::begin(cache));
        MergeExternal(ranges::begin(subrangeA),
                      ranges::end(subrangeA),
                      ranges::begin(subrangeB),
                      ranges::end(subrangeB),
                      ranges::begin(cache),
                      compare);
    }
}

constexpr auto merge_two_chunks_cache(auto&& subrangeA,
                                      auto&& subrangeB,
                                      auto&& cache_itr,
                                      auto&& compare)
{

    using difference_type = ranges::range_difference_t<
        std::remove_cvref_t<decltype(subrangeA)>>;

    // merge A1 and B1 into the cache
    if (compare(*(ranges::end(subrangeB) - 1),
                *ranges::begin(subrangeA))) {
        // the two ranges are in reverse order, so copy
        // them in reverse order into the cache
        ranges::copy(subrangeB, cache_itr);
        ranges::copy(subrangeA,
                     cache_itr + difference_type(ranges::size(subrangeB)));
    }
    else if (compare(*ranges::begin(subrangeB),
                     *(ranges::end(subrangeA) - 1))) {
        // these two ranges weren't already in order, so
        // merge them into the cache
        ranges::merge(subrangeA, subrangeB, cache_itr, compare);
    }
    else {

        // copy A1 and B1 into the cache in the same order
        // at once
        ranges::copy(
            ranges::begin(subrangeA), ranges::end(subrangeB), cache_itr);
    }
}

// merge 4 chunks at a time using an in memory cache
constexpr auto merge_four_chunks(auto&& subrangeA1,
                                 auto&& subrangeB1,
                                 auto&& subrangeA2,
                                 auto&& subrangeB2,
                                 auto&& cache,
                                 auto&& compare)
{
    // if A1, B1, A2, and B2 are all in order, skip
    // doing anything else
    if (!compare(*ranges::begin(subrangeB2),
                 *(ranges::end(subrangeA2) - 1)) &&
        !compare(*ranges::begin(subrangeA2),
                 *(ranges::end(subrangeB1) - 1))) {
        return;
    }

    // Next level
    auto level2_a_size = ranges::distance(ranges::begin(subrangeA1),
                                          ranges::end(subrangeB1));
    auto level2_b_size = ranges::distance(ranges::begin(subrangeA2),
                                          ranges::end(subrangeB2));
    auto l2_cache = ranges::begin(subrangeA1);

    // merge A1 and B1 into the cache
    merge_two_chunks_cache(std::forward<decltype(subrangeA1)>(subrangeA1),
                           std::forward<decltype(subrangeB1)>(subrangeB1),
                           ranges::begin(cache),
                           compare);

    // merge A2 and B2 into the cache
    merge_two_chunks_cache(std::forward<decltype(subrangeA2)>(subrangeA2),
                           std::forward<decltype(subrangeB2)>(subrangeB2),
                           ranges::begin(cache) + level2_a_size,
                           compare);

    // merge A1 and A2 from the cache into the array
    // A3 and B3 are in cache taken to A1 and A2 in the array
    // This is the next level of chunks merged ahead of time
    merge_two_chunks_cache(
        ranges::subrange(ranges::begin(cache),
                         ranges::begin(cache) + level2_a_size),
        ranges::subrange(ranges::begin(cache) + level2_a_size,
                         ranges::begin(cache) + level2_a_size +
                             level2_b_size),
        l2_cache,
        compare);
}

// sort using cache
constexpr auto block_sort_cache(auto&& iterator,
                                auto&& cache,
                                auto&& range,
                                auto&& compare)
{
    using RangeType = std::remove_cvref_t<decltype(range)>;
    using difference_type = ranges::range_difference_t<RangeType>;

    // if four subarrays fit into the cache, it's faster to merge
    // both pairs of subarrays into the cache, then merge the two
    // merged subarrays from the cache back into the original array
    if ((iterator.length() + 1) * 4 <=
            difference_type(ranges::size(cache)) &&
        iterator.length() * 4 <= difference_type(ranges::size(range))) {
        iterator.begin();
        while (!iterator.finished()) {
            merge_four_chunks(iterator.nextRange(range),
                              iterator.nextRange(range),
                              iterator.nextRange(range),
                              iterator.nextRange(range),
                              cache,
                              compare);
        }

        // we merged two levels at the same time, so we're done
        // with this level already (iterator.nextLevel() is called
        // again at the bottom of this outer merge loop)
        iterator.nextLevel();
    }
    else {
        iterator.begin();
        while (!iterator.finished()) {
            merge_two_chunks(iterator.nextRange(range),
                             iterator.nextRange(range),
                             cache,
                             compare);
        }
    }
}

constexpr auto Merge(auto&& a1,
                     auto&& a2,
                     auto&& b1,
                     auto&& b2,
                     auto&& buffer,
                     auto&& cache,
                     auto&& compare)
{

    using difference_type = std::iterator_traits<
        std::remove_cvref_t<decltype(a1)>>::difference_type;

    if (ranges::distance(a1, a2) <= difference_type(ranges::size(cache))) {
        MergeExternal(FWD(a1),
                      FWD(a2),
                      FWD(b1),
                      FWD(b2),
                      ranges::begin(cache),
                      compare);
    }
    else if (ranges::size(buffer) > 0) {
        MergeInternal(FWD(a1),
                      FWD(a2),
                      FWD(b1),
                      FWD(b2),
                      ranges::begin(buffer),
                      compare);
    }
    else {
        MergeInPlace(FWD(a1), FWD(a2), FWD(b1), FWD(b2), compare);
    }
}

constexpr auto merge_internal_buffers(auto&& buffers,
                                      auto&& subrangeA,
                                      auto&& subrangeB,
                                      auto&& cache,
                                      auto&& compare)
{
    using difference_type = ranges::range_difference_t<
        std::remove_cvref_t<decltype(subrangeA)>>;

    // break the remainder of A into blocks. firstA is the
    // uneven-sized first A block
    ranges::subrange firstA(ranges::begin(subrangeA),
                            ranges::begin(subrangeA) +
                                (difference_type(ranges::size(subrangeA)) %
                                 buffers.block_size));

    ranges::subrange ABlocks{ranges::end(firstA), ranges::end(subrangeA)};
    // swap the first value of each A block with the values
    // in buffer1
    ranges::swap_ranges(buffers.buffer1,
                        ABlocks |
                            ranges::views::stride(buffers.block_size));

    // start rolling the A blocks through the B blocks!
    // when we leave an A block behind we'll need to merge
    // the previous A block with any B blocks that follow
    // it, so track that information as well
    ranges::subrange lastA{firstA};
    ranges::subrange lastB{ranges::begin(subrangeB),
                           ranges::begin(subrangeB)};
    ranges::chunk_view BBlocks{subrangeB, buffers.block_size};

    auto indexA = ranges::begin(buffers.buffer1);

    // if the first unevenly sized A block fits into the
    // cache, copy it there for when we go to Merge it
    // otherwise, if the second buffer is available, block
    // swap the contents into that
    if (ranges::size(lastA) <= ranges::size(cache)) {
        ranges::copy(lastA, ranges::begin(cache));
    }
    else if (ranges::size(buffers.buffer2) > 0) {
        ranges::swap_ranges(lastA, buffers.buffer2);
    }

    auto blockB = ranges::begin(BBlocks);

    while (not ranges::empty(ABlocks)) {
        auto blockA = ABlocks | ranges::views::take(buffers.block_size);

        // if there's a previous B block and the first
        // value of the minimum A block is <= the last
        // value of the previous B block, then drop
        // that minimum A block behind. or if there are
        // no B blocks left then keep dropping the
        // remaining A blocks.
        if ((ranges::size(lastB) > 0 &&
             !compare(*(ranges::end(lastB) - 1), *indexA)) ||
            blockB == ranges::end(BBlocks)) {
            // figure out where to split the previous B
            // block, and rotate it at the split
            auto B_split = ranges::lower_bound(lastB, *indexA, compare);
            auto B_remaining = std::distance(B_split, ranges::end(lastB));

            // swap the minimum A block to the
            // beginning of the rolling A blocks
            {
                // we stride over each A block by block_size to get the
                // first element which the min element of that block.
                auto min_elems =
                    ABlocks | ranges::views::stride(buffers.block_size);

                // we find the minimum min element of each block, and swap
                // that
                ranges::swap_ranges(
                    blockA,
                    ranges::min_element(min_elems, compare).base());
            }
            // swap the first item of the previous A
            // block back with its original value,
            // which is stored in buffer1
            ranges::iter_swap(ranges::begin(blockA), indexA);
            ++indexA;

            // locally merge the previous A block with
            // the B values that follow it if lastA
            // fits into the external cache we'll use
            // that (with MergeExternal), or if the
            // second internal buffer exists we'll use
            // that (with MergeInternal), or failing
            // that we'll use a strictly in-place merge
            // algorithm (MergeInPlace)
            Merge(ranges::begin(lastA),
                  ranges::end(lastA),
                  ranges::end(lastA),
                  B_split,
                  buffers.buffer2,
                  cache,
                  compare);

            if (ranges::size(buffers.buffer2) > 0 ||
                buffers.block_size <=
                    difference_type(ranges::size(cache))) {
                // copy the previous A block into the
                // cache or buffer2, since that's where
                // we need it to be when we go to merge
                // it anyway
                if (buffers.block_size <=
                    difference_type(ranges::size(cache))) {
                    ranges::copy(blockA, ranges::begin(cache));
                }
                else {
                    ranges::swap_ranges(blockA,
                                        ranges::begin(buffers.buffer2));
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
                                 ranges::end(blockA) - B_remaining);
            }
            else {
                // we are unable to use the 'buffer2'
                // trick to speed up the rotation
                // operation since buffer2 doesn't
                // exist, so perform a normal rotation
                std::rotate(
                    B_split, ranges::begin(blockA), ranges::end(blockA));
            }

            // update the range for the remaining A
            // blocks, and the range remaining from the
            // B block after it was split
            lastA = shift_range(blockA, -B_remaining);
            lastB = ranges::subrange(ranges::end(lastA),
                                     ranges::end(lastA) + B_remaining);

            // if there are no more A blocks remaining,
            // this step is finished!
            ABlocks = adjust_begin(ABlocks, buffers.block_size);
        }
        else if (difference_type(ranges::size(*blockB)) <
                 buffers.block_size) {
            // move the last B block, which is unevenly
            // sized, to before the remaining A blocks,
            // by using a rotation
            ranges::rotate(ranges::begin(ABlocks),
                           ranges::begin(*blockB),
                           ranges::end(*blockB));

            lastB = ranges::subrange(
                ranges::begin(ABlocks),
                ranges::begin(ABlocks) +
                    difference_type(ranges::size(*blockB)));
            ABlocks = shift_range(ABlocks, ranges::size(*blockB));
            ++blockB;
        }
        else {
            // roll the leftmost A block to the end by
            // swapping it with the next B block
            ranges::swap_ranges(blockA, *blockB);
            lastB = ranges::subrange(blockA);

            ABlocks = shift_range(ABlocks, buffers.block_size);

            ++blockB;
        }
    }

    // merge the last A block with the remaining B values
    Merge(ranges::begin(lastA),
          ranges::end(lastA),
          ranges::end(lastA),
          ranges::end(subrangeB),
          buffers.buffer2,
          cache,
          compare);
}

constexpr auto block_sort_internal(auto&& iterator,
                                   auto&& cache,
                                   auto&& range,
                                   auto&& compare)
{

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

    InternalBuffers buffers{iterator, range, cache, compare};

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

        if (compare(*(ranges::end(subrangeB) - 1),
                    *ranges::begin(subrangeA))) {
            // the two ranges are in reverse order, so a simple
            // rotation should fix it
            std::rotate(ranges::begin(subrangeA),
                        ranges::end(subrangeA),
                        ranges::end(subrangeB));
        }
        else if (compare(*ranges::end(subrangeA),
                         *(ranges::end(subrangeA) - 1))) {
            // these two ranges weren't already in order, so we'll
            // need to merge them!
            merge_internal_buffers(
                buffers, subrangeA, subrangeB, cache, compare);
        }
    }

    // clean up internal buffers
    buffers.clean_up(compare);
}

// bottom-up merge sort combined with an in-place merge algorithm for O(1)
// memory use
template <ranges::contiguous_range RANGE,
          typename Comparison = std::less<>>
constexpr auto algorithm(RANGE range,
                         Comparison const& compare = {},
                         std::optional<size_t> const& cache_size_opt = {})
    -> RANGE
{
    // map first and last to a C-style array, so we don't have to change
    // the rest of the code (bit of a nasty hack, but it's good enough for
    // now...)

    using T = ranges::range_value_t<RANGE>;
    using difference_type = ranges::range_difference_t<RANGE>;

    // if the array is of size 0, 1, 2, or 3, just sort them like so:
    if (ranges::size(range) < 4) {
        sort_four(range, compare);
        return range;
    }

    // sort groups of 4-8 items at a time using an unstable sorting
    // network, but keep track of the original item orders to force it to
    // be stable http://pages.ripco.net/~jgamble/nw.html
    Iterator<RANGE> iterator(difference_type(ranges::size(range)), 4);

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
        if (iterator.length() < difference_type(ranges::size(cache))) {
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

template <class Ordering>
struct _adapter final
{
    struct type;
};

template <class Ordering>
using adapter = _adapter<std::remove_cvref_t<Ordering>>::type;

namespace _cpo
{
constexpr struct _fn
{

    static constexpr auto operator()(
        auto const& compare,
        std::optional<std::size_t> const& cache_size = std::nullopt)
    {
        return adapter<decltype(compare)>{cache_size};
    }

    template <class Ordering>
    static constexpr auto operator()(
        ranges::contiguous_range auto&& range,
        Ordering&& ordering = ordering::ascending{},
        std::optional<size_t> const& cache_size = std::nullopt)
    {
        return tag_invoke(_fn{}, FWD(range), FWD(ordering), cache_size);
    }

private:
    template <class Ordering>
    friend constexpr auto tag_invoke(_fn const& /*unused*/,
                                     ranges::contiguous_range auto&& range,
                                     Ordering const& /*ordering*/,
                                     auto&& cache_size)
    {
        return algorithm(FWD(range),
                         predicate_for_t<Ordering, RNG_VALUE_T(range)>{},
                         FWD(cache_size));
    }
} block_sort;
} // namespace _cpo

} // namespace _block_sort

using _block_sort::_cpo::block_sort;

namespace _block_sort
{

template <class Ordering>
struct _adapter<Ordering>::type final
{
    std::optional<size_t> cache_size;

    template <class Adapter>
    requires(std::same_as<std::remove_cvref_t<Adapter>, type>)
    friend constexpr auto operator|(ranges::contiguous_range auto&& range,
                                    Adapter&& adapter)
    {
        return tag_invoke(block_sort,
                          FWD(range),
                          Ordering{},
                          std::forward<Adapter>(adapter).cache_size);
    }

    constexpr auto operator()(this auto&& self,
                              ranges::contiguous_range auto&& range)
    {
        return tag_invoke(
            block_sort, FWD(range), Ordering{}, FWD(self).cache_size);
    }

    constexpr auto operator()(size_t cache_sz) const
    {
        return type{cache_sz};
    }

    constexpr auto operator()(ranges::contiguous_range auto&& range,
                              size_t cache_sz) const
    {
        return tag_invoke(block_sort, FWD(range), Ordering{}, cache_sz);
    }
};

} // namespace _block_sort
constexpr auto block_sort_ascending = block_sort(ordering::ascending{});
constexpr auto block_sort_descending = block_sort(ordering::descending{});

} // namespace algo

#include <algo/prologue.hpp>
