/**
 * Copyright (C) 2023  Todd W. Fulton
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <https://www.gnu.org/licenses/>.
 **/

#include "rand_range.hpp"

#include <gtest/gtest.h>

#include <algo/search.hpp>
#include <algo/sort.hpp>

#include <list>
#include <random>
#include <vector>

using std::mt19937;
using std::uniform_int_distribution;
using std::vector;

TEST(Searching, LinearSearchRandom_Vector)
{

    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<size_t> idx_distrib(0, range_size - 1);

    auto given = testing::rand_range() | algo::quick_sort();

    auto rand_idx = idx_distrib(gen);
    auto key = given[rand_idx];

    while (rand_idx >= 0 and given[rand_idx - 1] == given[rand_idx]) {
        --rand_idx;
    }

    auto expected = std::begin(given) + ptrdiff_t(rand_idx);

    EXPECT_EQ(algo::linear_search(given, key), expected);
}

TEST(Searching, LinearSearchRandom_List)
{

    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<size_t> idx_distrib(0, range_size - 1);

    auto given = testing::rand_range<std::list>() | algo::bubble_sort();

    auto rand_idx = idx_distrib(gen);
    auto expected = ranges::begin(given);
    std::advance(expected, rand_idx);

    for(auto itr = ranges::begin(given); itr != ranges::end(given); ++itr) {
        if (*itr == *expected) {
            expected = itr;
            break;
        }
    }

    EXPECT_EQ(algo::linear_search(given, *expected), expected);
}

TEST(Searching, BinarySearchRandom_Vector)
{

    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<size_t> idx_distrib(0, range_size - 1);

    auto given = testing::rand_range() | algo::quick_sort();

    auto rand_idx = idx_distrib(gen);
    auto key = given[rand_idx];

    while (rand_idx >= 0 and given[rand_idx - 1] == given[rand_idx]) {
        --rand_idx;
    }

    auto expected = begin(given) + ptrdiff_t(rand_idx);

    EXPECT_EQ(algo::binary_search(given, key), expected);
}

TEST(Searching, BinarySearchReversedRandom_Vector)
{
    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<size_t> idx_distrib(0, range_size - 1);

    auto given = testing::rand_range() | algo::quick_sort(algo::ordering::descending);

    auto rand_idx = idx_distrib(gen);
    auto key = given[rand_idx];

    while (rand_idx >= 0 and given[rand_idx - 1] == given[rand_idx]) {
        --rand_idx;
    }

    auto expected = begin(given) + ptrdiff_t(rand_idx);

    EXPECT_EQ(algo::binary_search(given, key), expected);
}
