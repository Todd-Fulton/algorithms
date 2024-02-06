include(GNUInstallDirs)

add_library(${PROJECT_NAME} INTERFACE)
add_library(mythos::${PROJECT_NAME} ALIAS ${PROJECT_NAME})

set_target_properties(${PROJECT_NAME} PROPERTIES CXX_VISIBILITY_PRESET hidden
                                                 VISIBILITY_INLINES_HIDDEN YES)

set(${PROJECT_NAME}_LIBFMT_VERSION_NEEDED 1.0.1)

find_package(fmt ${${PROJECT_NAME}_LIBFMT_VERSION_NEEDED} CONFIG REQUIRED)
find_package(range-v3 CONFIG REQUIRED)
find_package(unifex CONFIG REQUIRED)
find_package(Microsoft.GSL CONFIG REQUIRED)

if("${CMAKE_SYSTEM_NAME}" STREQUAL "Linux")
  find_package(PkgConfig)
  pkg_check_modules(liburing REQUIRED IMPORTED_TARGET GLOBAL liburing>=2.0)

  target_link_libraries(${PROJECT_NAME} INTERFACE PkgConfig::liburing)
endif()

target_link_libraries(
  ${PROJECT_NAME}
  INTERFACE algorithms_options
            algorithms_warnings
            fmt::fmt
            range-v3::meta
            range-v3::concepts
            range-v3::range-v3
            unifex::unifex
            Microsoft.GSL::GSL)

target_include_directories(
  ${PROJECT_NAME}
  INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_LIST_DIR}/include>
            $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/include>
            $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}> # <prefix>/include/
)

add_subdirectory(include)
add_subdirectory(src)

if(ALGORITHMS_TESTING)
  add_subdirectory(tests)
endif()

if(ALGORITHMS_EXAMPLES)
  add_subdirectory(examples)
endif()

if(ALGORITHMS_BENCHMARKS)
  add_subdirectory(benchmarks)
endif()

# Installation

set_target_properties(
  ${PROJECT_NAME} PROPERTIES VERSION ${PROJECT_VERSION}
                             SOVERSION ${PROJECT_VERSION_MAJOR})

if(CMAKE_SKIP_INSTALL_RULES)
  return()
endif()

include(CMakePackageConfigHelpers)

install(
  TARGETS ${PROJECT_NAME} ${PROJECT_NAME}_options ${PROJECT_NAME}_warnings
  EXPORT ${PROJECT_NAME}_Exports
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
          COMPONENT ${PROJECT_NAME}_Runtime
          NAMELINK_COMPONENT ${PROJECT_NAME}_Development
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
          COMPONENT ${PROJECT_NAME}_Development
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR} COMPONENT ${PROJECT_NAME}_Runtime
  INCLUDES
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  COMPONENT ${PROJECT_NAME}_Development
  PUBLIC_HEADER
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    COMPONENT ${PROJECT_NAME}_Development
    FILE_SET ${PROJECT_NAME}_HEADERS
    DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    COMPONENT ${PROJECT_NAME}_Development)

install(
  EXPORT ${PROJECT_NAME}_Exports
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}-${PROJECT_VERSION}
  NAMESPACE mythos::)

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/algorithms_export.h
        DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}/algorithms)

include(CMakePackageConfigHelpers)

configure_package_config_file(
  "${CMAKE_CURRENT_LIST_DIR}/${PROJECT_NAME}Config.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION
    ${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}-${PROJECT_VERSION})

write_basic_package_version_file(
  ${PROJECT_NAME}ConfigVersion.cmake
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY SameMajorVersion)

install(
  FILES "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
        "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}-${PROJECT_VERSION}"
)

# Experience shows that explicit package naming can help make it easier to sort
# out potential ABI related issues before they start, while helping you track a
# build to a specific GIT SHA
set(CPACK_PACKAGE_FILE_NAME
    "${CMAKE_PROJECT_NAME}-${CMAKE_PROJECT_VERSION}-${GIT_SHORT_SHA}-${CMAKE_SYSTEM_NAME}-${CMAKE_BUILD_TYPE}-${CMAKE_CXX_COMPILER_ID}-${CMAKE_CXX_COMPILER_VERSION}"
)

include(CPack)
