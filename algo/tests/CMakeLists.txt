enable_testing()

find_package(GTest CONFIG REQUIRED)

include(GoogleTest)

function(algo_add_tests name)
  add_executable(${name} ${name}.cpp
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/gtests_main.cpp
    ${CMAKE_CURRENT_FUNCTION_LIST_DIR}/rand_range.hpp
  )
  target_include_directories(${name}
    PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_FUNCTION_LIST_DIR}>
  )
  target_link_libraries(${name} PRIVATE algorithms GTest::gtest)

  gtest_discover_tests(${name})
endfunction()

algo_add_tests(sort_tests)
algo_add_tests(search_tests)
algo_add_tests(partition_tests)

add_subdirectory(containers)
# add_subdirectory(math)
# add_subdirectory(meta)
