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

TEST(Sorting, InsertSort_Ascending_Vector)
{
    const vector expected{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::insertion_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, InsertSort_Ascending_List)
{
    const list expected{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    vector<int> shuffled{expected.begin(), expected.end()};

    mt19937 gen{seed}; // NOLINT

    shuffle(shuffled, gen);

    list<int> given{shuffled.begin(), shuffled.end()};

    algo::insertion_sort(given);

    EXPECT_EQ(given, expected);
}

TEST(Sorting, InsertSort_Descending_Vector)
{
    const vector expected{9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
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
    const vector expected{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::merge_sort(given);

    EXPECT_EQ(given, expected);
}


TEST(Sorting, MergeSort_Descending_Vector)
{
    const vector expected{9, 8, 7, 6, 5, 4, 3, 2, 1, 0};
    vector<int> given{expected};

    mt19937 gen{seed}; // NOLINT

    shuffle(given, gen);

    algo::merge_sort(given, greater<>{});

    EXPECT_EQ(given, expected);
}

