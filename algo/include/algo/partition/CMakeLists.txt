target_sources(${PROJECT_NAME}
  INTERFACE
  FILE_SET ${PROJECT_NAME}_HEADERS
  TYPE HEADERS
    FILES
    alexandrescu_partition.hpp
    lomuto_partition.hpp
    hoare_partition.hpp
  )
