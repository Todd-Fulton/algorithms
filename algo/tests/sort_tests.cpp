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

#include <gtest/gtest.h>

#include <algo/sort.hpp>
#include <range/v3/algorithm.hpp>

#include "rand_range.hpp"
#include <algo/sort/intro_sort.hpp>

#include <list>
#include <vector>

using ranges::shuffle;
using std::greater;
using std::list;
using std::mt19937;
using std::vector;
using testing::rand_range;

TEST(Sorting, InsertSort_Ascending_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::insertion_sort_ascending));
}

TEST(Sorting, InsertSort_Ascending_List)
{
    EXPECT_TRUE(
        ranges::is_sorted(rand_range<std::list>() | algo::insertion_sort_ascending));
}

TEST(Sorting, InsertSort_Descending_Vector)
{
    auto compare = algo::predicate_for_t<algo::ordering::descending, int>{};
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::insertion_sort_descending,
                                  compare)); // NOLINT
}

TEST(Sorting, InsertSort_Descending_List)
{
    auto compare = algo::predicate_for_t<algo::ordering::descending, int>{};
    EXPECT_TRUE(
        ranges::is_sorted(rand_range<std::list>() | algo::insertion_sort_descending,
                          compare)); // NOLINT
}

TEST(Sorting, MergeSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::merge_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, MergeSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::merge_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, BubbleSort_Ascending_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::bubble_sort_ascending));
}

TEST(Sorting, BubbleSort_Ascending_List)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<std::list>() | algo::bubble_sort_ascending));
}

TEST(Sorting, BubbleSort_Descending_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::bubble_sort_decending));
}

TEST(Sorting, BubbleSort_Descending_List)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<std::list>() | algo::bubble_sort_decending));
}

TEST(Sorting, HeapSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::heap_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, HeapSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::heap_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, QuickSort_Ascending_Vector_Hoare_Scheme)
{
    auto expected = rand_range<>();
    auto result = expected | algo::quick_sort(algo::ordering::ascending{});
    ranges::sort(expected);
    EXPECT_EQ(result, expected);
}

TEST(Sorting, QuickSort_Descending_Vector_Hoare_Scheme)
{
    auto compare = algo::predicate_for_t<algo::ordering::descending, int>{};
    auto expected = rand_range<>();
    auto result = expected | algo::quick_sort(algo::ordering::descending{});
    ranges::sort(expected, compare);
    EXPECT_EQ(result, expected);
}

TEST(Sorting, QuickSort_Ascending_Vector_Lomuto_Scheme)
{
    auto expected = rand_range<>();
    auto result =
        expected | algo::quick_sort(algo::ordering::ascending{}, algo::lomuto_partition);
    ranges::sort(expected);
    EXPECT_EQ(result, expected);
}

TEST(Sorting, QuickSort_Descending_Vector_Lomuto_Scheme)
{
    auto compare = algo::predicate_for_t<algo::ordering::descending, int>{};
    auto expected = rand_range<>();
    auto result =
        expected | algo::quick_sort(algo::ordering::descending{}, algo::lomuto_partition);
    ranges::sort(expected, compare);
    EXPECT_EQ(result, expected);
}

TEST(Sorting, QuickSort_Ascending_Vector_Branchless_Lomuto_Scheme)
{
    auto expected = rand_range<>();
    auto result = expected | algo::quick_sort(algo::ordering::ascending{},
                                              algo::branchless_lomuto_partition);
    ranges::sort(expected);
    EXPECT_EQ(result, expected);
}

TEST(Sorting, QuickSort_Descending_Vector_Branchless_Lomuto_Scheme)
{
    auto compare = algo::predicate_for_t<algo::ordering::descending, int>{};
    auto expected = rand_range<>();
    auto result = expected | algo::quick_sort(algo::ordering::descending{},
                                              algo::branchless_lomuto_partition);
    ranges::sort(expected, compare);
    EXPECT_EQ(result, expected);
}

TEST(Sorting, ShellSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::shell_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, ShellSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::shell_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, SelectionSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::selection_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, SelectionSort_Ascending_List)
{
    const list expected{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::selection_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, SelectionSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::selection_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, SelectionSort_Descending_List)
{
    const list expected{9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::selection_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, TimSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::tim_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, TimSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::tim_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, TreeSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::tree_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, TreeSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::tree_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, IntroSort_Descending_Vector)
{

    auto given = rand_range<>();
    auto expected = given;
    ranges::sort(expected);

    algo::intro_sort(given, std::less<int>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, BlockSort_Ascending_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::block_sort_ascending,
                                  algo::predicate_for_t<algo::ordering::ascending>{}));
}

TEST(Sorting, BlockSort_Descending_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::block_sort_descending,
                                  algo::predicate_for_t<algo::ordering::descending>{}));
}

TEST(Sorting, BlockSort_Ascending_NoCache_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::block_sort_ascending(0),
                                  algo::predicate_for_t<algo::ordering::ascending>{}));
}

TEST(Sorting, BlockSort_Descending_NoCache_Vector)
{
    EXPECT_TRUE(ranges::is_sorted(rand_range<>() | algo::block_sort_descending(0),
                                  algo::predicate_for_t<algo::ordering::descending>{}));
}

TEST(Sorting, BlockSort_Ascending_SmallCache_Vector)
{
    EXPECT_TRUE(
        ranges::is_sorted(rand_range<>() | algo::block_sort_ascending(range_size / 4 + 1),
                          algo::predicate_for_t<algo::ordering::ascending>{}));
}

TEST(Sorting, BlockSort_Descending_SmallCache_Vector)
{

    EXPECT_TRUE(ranges::is_sorted(rand_range<>() |
                                      algo::block_sort_descending(range_size / 4 + 1),
                                  algo::predicate_for_t<algo::ordering::descending>{}));
}
