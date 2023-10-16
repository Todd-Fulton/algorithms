target_sources(${PROJECT_NAME}
  INTERFACE
  FILE_SET ${PROJECT_NAME}_HEADERS
  TYPE HEADERS
    FILES
    sort.hpp
    search.hpp
  )

add_subdirectory(sort)
add_subdirectory(search)
