enable_testing()

find_package(GTest CONFIG REQUIRED)

include(GoogleTest)

add_executable(sort_tests sort_tests.cpp)
target_link_libraries(sort_tests PRIVATE algorithms GTest::gtest GTest::gtest_main)

gtest_discover_tests(sort_tests)

