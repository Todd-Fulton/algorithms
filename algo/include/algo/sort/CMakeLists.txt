
target_sources(${PROJECT_NAME}
  INTERFACE
  FILE_SET ${PROJECT_NAME}_HEADERS
  TYPE HEADERS
    FILES
    block_sort.hpp
    insertion_sort.hpp
    merge_sort.hpp
  )
