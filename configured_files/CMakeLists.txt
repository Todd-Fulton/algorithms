# A very simple example of a configured file that might need to be
# converted to one that is publicly installed in the case that
# you are developing a library
configure_file(
  "config.hpp.in"
  "${CMAKE_BINARY_DIR}/algo/include/algo/config.hpp"
  ESCAPE_QUOTES)

add_library(
  ${PROJECT_NAME}_config
  INTERFACE
  "${CMAKE_BINARY_DIR}/algo/include/algo/config.hpp"
)
add_library(mythos::${PROJECT_NAME}_config ALIAS ${PROJECT_NAME}_config)

set_target_properties(
  ${PROJECT_NAME}_config
  PROPERTIES
  CXX_VISIBILITY_PRESET hidden
  VISIBILITY_INLINES_HIDDEN YES
)

target_include_directories(${PROJECT_NAME}_config
  INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}> # <prefix>/include/
)

