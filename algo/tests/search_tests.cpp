#include <gtest/gtest.h>

#include <algo/sort.hpp>
#include <algo/search.hpp>

#include <algorithm>
#include <list>
#include <random>
#include <vector>

extern unsigned long long seed; // NOLINT

using std::mt19937;
using std::uniform_int_distribution;
using std::vector;
using std::ranges::for_each;

static constexpr size_t TEST_RANGE_SIZE = 20;

TEST(Searching, LinearSearchRandom_Vector)
{

    vector<int> given{};
    given.resize(TEST_RANGE_SIZE);

    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<> val_distrib{0, 10000};
    uniform_int_distribution<size_t> idx_distrib(0, TEST_RANGE_SIZE - 1);

    for_each(given, [&](auto& x) { x = val_distrib(gen); });

    auto rand_idx = idx_distrib(gen);
    auto key = given[rand_idx];

    auto expected = std::begin(given) + ptrdiff_t(rand_idx);

    EXPECT_EQ(algo::linear_search(given, key), expected);
}

TEST(Searching, BinarySearchRandom_Vector)
{

    vector<int> given{};
    given.resize(TEST_RANGE_SIZE);

    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<> val_distrib{0, 10000};
    uniform_int_distribution<size_t> idx_distrib(0, TEST_RANGE_SIZE - 1);

    for_each(given, [&](auto& x) { x = val_distrib(gen); });
    algo::merge_sort(given);

    auto rand_idx = idx_distrib(gen);
    auto key = given[rand_idx];

    auto expected = std::begin(given) + ptrdiff_t(rand_idx);

    EXPECT_EQ(algo::binary_search(given, key), expected);
}

TEST(Searching, BinarySearchRandom_List)
{

    std::list<int> given{};

    mt19937 gen{seed}; // NOLINT
    uniform_int_distribution<> val_distrib{0, 10000};
    uniform_int_distribution<size_t> idx_distrib(0, TEST_RANGE_SIZE - 1);

    for_each(std::ranges::iota_view(0ULL, TEST_RANGE_SIZE),
             [&]([[maybe_unused]] auto const& _) {
                 given.emplace_back(val_distrib(gen));
             });

    algo::insertion_sort(given);

    auto rand_idx = idx_distrib(gen);

    auto expected = std::begin(given);
    while(rand_idx != 0) {
        ++expected;
        --rand_idx;
    }

    auto key = *expected;

    EXPECT_EQ(algo::binary_search(given, key), expected);
}
