#include <gtest/gtest.h>

#include <algo/structures/dynamic_array.hpp>

#include "rand_range.hpp"

using testing::rand_int;
using testing::rand_range;
using namespace algo;

TEST(Structures, dynamic_array_default_ctr)
{
    dynamic_array<int> arr;

    EXPECT_EQ(size(arr), 0);
    EXPECT_EQ(capacity(arr), 0);
}

TEST(Structures, dynamic_array_reserve_n)
{
    dynamic_array<int> arr;
    const auto n = rand_int<size_t>();
    reserve(arr, n);
    EXPECT_GE(capacity(arr), n);
    EXPECT_EQ(size(arr), 0);
}

namespace
{
consteval void _test_dynamic_array_reserve_constexpr()
{
    dynamic_array<int> arr;
    reserve(arr, 10);
}
} // namespace

TEST(Structures, dynamic_array_reserve_constexpr)
{
    _test_dynamic_array_reserve_constexpr();
    EXPECT_TRUE(true);
}

TEST(Structures, dynamic_array_insert_n)
{
    dynamic_array<int> arr;
    auto r = rand_range();

    reserve(arr, r.size());

    auto cur = first(arr);
    for (auto i : r) {
        insert(arr, cur, i);
        inc(arr, cur);
    }

    cur = first(arr);
    for (auto i : r) {
        EXPECT_EQ(read_at(arr, cur), i);
        inc(arr, cur);
    }
}
