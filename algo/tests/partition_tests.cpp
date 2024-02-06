
#include <gtest/gtest.h>

#include <algo/partition.hpp>

#include "rand_range.hpp"

#include <range/v3/algorithm/is_partitioned.hpp>

#include <list>
#include <range/v3/algorithm/partition_point.hpp>
#include <range/v3/algorithm/sort.hpp>
#include <vector>

using std::list;
using std::vector;
using testing::rand_range;

TEST(Partitions, LomutoPartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(
        ranges::is_partitioned(std::get<1>(rand_range<vector>(0, 50) |
                                           algo::lomuto_partition(pred)),
                               pred));
}

TEST(Partitions, LomutoPartition_list_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(
        ranges::is_partitioned(std::get<1>(rand_range<list>(0, 50) |
                                           algo::lomuto_partition(pred)),
                               pred));
}

TEST(Partitions, BranchlessLomutoPartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(rand_range<vector>(0, 50) |
                    algo::branchless_lomuto_partition(pred)),
        pred));
}

TEST(Partitions, HoarePartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(
        ranges::is_partitioned(std::get<1>(rand_range<vector>(0, 50) |
                                           algo::hoare_partition(pred)),
                               pred));
}

TEST(Partitions, AlexandrescuPartition_vector_int)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(rand_range<vector>(0, 50) |
                    algo::alexandrescu_partition(pred)),
        pred));
}

TEST(Partitions, AlexandrescuPartition_vector_int_Already_Partitioned)
{
    constexpr auto pred = [](auto const& x) { return x <= 20; };

    EXPECT_TRUE(ranges::is_partitioned(
        std::get<1>(std::get<1>(rand_range<vector>(0, 50) |
                                algo::alexandrescu_partition(pred)) |
                    algo::alexandrescu_partition(pred)),
        pred));
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
        std::get<1>(ranges::views::all(range) |
                    algo::alexandrescu_partition(pred)),
        pred));

    auto new_pivot = ranges::partition_point(range, pred);

    EXPECT_EQ(expected_pivot, new_pivot);

    ranges::sort(begin(range), new_pivot);
    ranges::sort(new_pivot, end(range));

    EXPECT_EQ(range, expected);
}