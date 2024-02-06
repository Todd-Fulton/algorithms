enable_testing()

find_package(GTest CONFIG REQUIRED)

include(GoogleTest)

macro(algo_add_tests name)
  add_executable(${name} ${name}.cpp gtests_main.cpp rand_range.hpp)
  target_link_libraries(${name} PRIVATE algorithms GTest::gtest)

  gtest_discover_tests(${name})
endmacro()

algo_add_tests(sort_tests)
algo_add_tests(search_tests)
algo_add_tests(partition_tests)
