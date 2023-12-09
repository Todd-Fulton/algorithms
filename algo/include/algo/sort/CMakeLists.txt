
target_sources(${PROJECT_NAME}
  INTERFACE
  FILE_SET ${PROJECT_NAME}_HEADERS
  TYPE HEADERS
    FILES
    block_sort.hpp
    insertion_sort.hpp
    merge_sort.hpp
    bubble_sort.hpp
    cube_sort.hpp
    flux_sort.hpp
    heap_sort.hpp
    intro_sort.hpp
    quick_sort.hpp
    selection_sort.hpp
    shell_sort.hpp
    tim_sort.hpp
    tree_sort.hpp
  )
