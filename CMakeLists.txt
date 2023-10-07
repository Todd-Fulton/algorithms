cmake_minimum_required(VERSION 3.28)

option(ALGORITHMS_TESTING "Build the tests" OFF)
option(ALGORITHMS_BENCHMARKS "Build the tests" OFF)
option(ALGORITHMS_EXAMPLES "Build the examples" OFF)
option(BUILD_SHARED_LIBS "Build using shared libraries" OFF)

if(ALGORITHMS_TESTING)
  list(APPEND VCPKG_MANIFEST_FEATURES "tests")
endif()

if(ALGORITHMS_BENCHMARKS)
  list(APPEND VCPKG_MANIFEST_FEATURES "benchmarks")
endif()

# strongly encouraged to enable this globally to avoid conflicts between
# -Wpedantic being enabled and -std=c++20 and -std=gnu++20 for example
# when compiling with PCH enabled
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 26)

# Set the project name and language
project(
  algorithms
  VERSION 0.0.1
  DESCRIPTION "Algorithms for C++"
  #HOMEPAGE_URL "%%myurl%%"
  LANGUAGES CXX C)

include(cmake/PreventInSourceBuilds.cmake)
include(ProjectOptions.cmake)
algorithms_setup_options()
algorithms_global_options()
algorithms_local_options()

set(GIT_SHA
  "Unknown"
  CACHE STRING "SHA this build was generated from")
string(
  SUBSTRING "${GIT_SHA}"
  0
  8
  GIT_SHORT_SHA)

# target_link_libraries(algorithms_options INTERFACE clang_rt.ubsan_standalone)

add_library(algorithms::algorithms_options INTERFACE IMPORTED)
add_library(algorithms::algorithms_warnings INTERFACE IMPORTED)


# configure files based on CMake configuration options
add_subdirectory(configured_files)

# include(cmake/PackageProject.cmake)
add_subdirectory(algo)


# probably move this under algorithms
# if(algorithms_BUILD_FUZZ_TESTS)
#   message(AUTHOR_WARNING "Building Fuzz Tests, using fuzzing sanitizer https://www.llvm.org/docs/LibFuzzer.html")
#   if (NOT algorithms_ENABLE_ADDRESS_SANITIZER AND NOT algorithms_ENABLE_THREAD_SANITIZER)
#     message(WARNING "You need asan or tsan enabled for meaningful fuzz testing")
#   endif()
#   add_subdirectory(fuzz_test)
# endif()

# If MSVC is being used, and ASAN is enabled, we need to set the debugger environment
# so that it behaves well with MSVC's debugger, and we can run the target from visual studio
# if(MSVC)
#   get_all_installable_targets(all_targets)
#   message("all_targets=${all_targets}")
#   set_target_properties(${all_targets} PROPERTIES VS_DEBUGGER_ENVIRONMENT "PATH=$(VC_ExecutablePath_x64);%PATH%")
# endif()
