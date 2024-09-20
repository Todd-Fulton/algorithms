
#include <fmt/core.h>
#include <fmt/ranges.h>

#include <gtest/gtest.h>

#include <algo/partition.hpp>

#include "rand_range.hpp"

#include <range/v3/algorithm/for_each.hpp>
#include <range/v3/algorithm/is_partitioned.hpp>

#include <list>
#include <range/v3/algorithm/partition_point.hpp>
#include <range/v3/algorithm/sort.hpp>
#include <range/v3/view/chunk.hpp>
#include <range/v3/view/stride.hpp>

#include <vector>

using std::list;
using std::vector;
using testing::rand_range;

TEST(Partitions, LomutoPartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(rand_range<vector>(0, 50) | algo::lomuto_partition(pred)),
        pred)); // NOLINT
}

TEST(Partitions, LomutoPartition_list_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(rand_range<list>(0, 50) | algo::lomuto_partition(pred)),
        pred)); // NOLINT
}

TEST(Partitions, BranchlessLomutoPartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(rand_range<vector>(0, 50) | algo::branchless_lomuto_partition(pred)),
        pred)); // NOLINT
}

TEST(Partitions, HoarePartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(rand_range<vector>(0, 50) | algo::hoare_partition(pred)),
        pred)); // NOLINT
}

TEST(Partitions, HoarePartition_vector_int_iterators)
{
    constexpr auto pred = [](auto const& x) { return x <= 10; };

    auto given = rand_range<>(-20, 20);
    algo::hoare_partition(begin(given), end(given), pred);
    EXPECT_TRUE(ranges::is_partitioned(given, pred));
}

TEST(Partitions, AlexandrescuPartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 25; };

    auto rrand = rand_range<>(0, 50);
    auto original = rrand;

    auto [part, output] = original | algo::alexandrescu_partition(pred);

    auto is_partitioned = ranges::is_partitioned(output, pred); // NOLINT

    EXPECT_TRUE(is_partitioned);
    if (!is_partitioned) {
        fmt::print("Original: {}\n\n", original);
        fmt::print("Output:   {}\n\n", output);
    }
}

TEST(Partitions, AlexandrescuPartition_vector_int_Already_Partitioned)
{
    constexpr auto pred = [](auto const& x) { return x <= 25; };

    auto original = rand_range<>(0, 50);
    auto result = original;

    algo::alexandrescu_partition(begin(result), end(result), pred);
    algo::alexandrescu_partition(ranges::views::all(result), pred);

    auto is_partitioned = ranges::is_partitioned(result, pred);

    EXPECT_TRUE(is_partitioned);
    if (!is_partitioned) {
        fmt::print("Original: {}\n\n", original);
        fmt::print("Result:   {}\n\n", result);
    }
}

TEST(Partitions, AlexandrescuPartition_vector_int_Degenerate)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    auto range = rand_range<vector>(0, 50);
    ranges::sort(range);

    auto expected = range;

    auto expected_pivot = ranges::partition_point(range, pred);

    ranges::iter_swap(begin(range), prev(end(range)));

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(ranges::views::all(range) 
            | algo::alexandrescu_partition(pred)), pred)); // NOLINT

    auto new_pivot = ranges::partition_point(range, pred);

    EXPECT_EQ(ranges::distance(ranges::begin(range), expected_pivot),
              ranges::distance(ranges::begin(range), new_pivot));

    ranges::sort(begin(range), new_pivot);
    ranges::sort(new_pivot, end(range));

    EXPECT_EQ(range, expected);
}
