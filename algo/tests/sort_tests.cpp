#include <gtest/gtest.h>

#include <algo/sort.hpp>

#include <vector>
#include <list>

using std::vector;
using std::list;
using std::greater;


TEST(Sorting, InsertSort_Ascending_Vector) {
  vector given{9,2,8,4,6,3,1,0,7,5};
  const vector expected{0,1,2,3,4,5,6,7,8,9};

  algo::insertion_sort(given);

  EXPECT_EQ(given, expected);

}

TEST(Sorting, InsertSort_Ascending_List) {
  list given{9,2,8,4,6,3,1,0,7,5};
  const list expected{0,1,2,3,4,5,6,7,8,9};

  algo::insertion_sort(given);

  EXPECT_EQ(given, expected);

}

TEST(Sorting, InsertSort_Descending_Vector) {
  vector given{9,2,8,4,6,3,1,0,7,5};
  const vector expected{9,8,7,6,5,4,3,2,1,0};

  algo::insertion_sort(given, greater<>{});

  EXPECT_EQ(given, expected);

}

TEST(Sorting, InsertSort_Descending_List) {
  list given{9,2,8,4,6,3,1,0,7,5};
  const list expected{9,8,7,6,5,4,3,2,1,0};

  algo::insertion_sort(given, greater<>{});

  EXPECT_EQ(given, expected);

}
