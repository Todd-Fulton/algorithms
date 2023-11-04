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

#include <gtest/gtest.h>

#include <algo/sort.hpp>

#include <algorithm>
#include <list>
#include <random>
#include <vector>

using std::greater;
using std::list;
using std::mt19937;
using std::vector;
using std::ranges::shuffle;

extern unsigned long long seed; // NOLINT
extern unsigned long long range_size; // NOLINT


TEST(Sorting, InsertSort_Ascending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::insertion_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, InsertSort_Ascending_List)
{
    list<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::insertion_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, InsertSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::insertion_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, InsertSort_Descending_List)
{
    const list expected{9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::insertion_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
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
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::bubble_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, BubbleSort_Ascending_List)
{
    const list expected{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::bubble_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, BubbleSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::bubble_sort(given, std::less<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, BubbleSort_Descending_List)
{
    const list expected{9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::bubble_sort(given, std::less<>{});

    EXPECT_EQ(given, expected);
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

TEST(Sorting, QuickSort_Ascending_Vector) {
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::quick_sort(given);

    EXPECT_EQ(given, expected);
}


TEST(Sorting, QuickSort_Descending_Vector)
{
    vector<int> expected(range_size);
    std::iota(std::begin(expected), std::end(expected), 0);
    std::ranges::reverse(expected);
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::quick_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

TEST(Sorting, ShellSort_Ascending_Vector) {
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


TEST(Sorting, TimSort_Ascending_Vector) {
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
