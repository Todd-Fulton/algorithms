add_executable(linear_search linear_search.cpp)
target_link_libraries(linear_search PRIVATE ${PROJECT_NAME})

add_executable(temp temp.cpp)
target_link_libraries(temp PRIVATE ${PROJECT_NAME})
