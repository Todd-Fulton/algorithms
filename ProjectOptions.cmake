include(cmake/SystemLink.cmake)
include(cmake/LibFuzzer.cmake)
include(CMakeDependentOption)
include(CheckCXXCompilerFlag)


macro(algorithms_supports_sanitizers)
  if((CMAKE_CXX_COMPILER_ID MATCHES ".*Clang.*" OR CMAKE_CXX_COMPILER_ID MATCHES ".*GNU.*") AND NOT WIN32)
    set(SUPPORTS_UBSAN ON)
  else()
    set(SUPPORTS_UBSAN OFF)
  endif()

  if((CMAKE_CXX_COMPILER_ID MATCHES ".*Clang.*" OR CMAKE_CXX_COMPILER_ID MATCHES ".*GNU.*") AND WIN32)
    set(SUPPORTS_ASAN OFF)
  else()
    set(SUPPORTS_ASAN ON)
  endif()
endmacro()

macro(algorithms_setup_options)
  option(algorithms_ENABLE_HARDENING "Enable hardening" ON)
  option(algorithms_ENABLE_COVERAGE "Enable coverage reporting" OFF)
  cmake_dependent_option(
    algorithms_ENABLE_GLOBAL_HARDENING
    "Attempt to push hardening options to built dependencies"
    ON
    algorithms_ENABLE_HARDENING
    OFF)

  algorithms_supports_sanitizers()

  if(NOT PROJECT_IS_TOP_LEVEL OR algorithms_PACKAGING_MAINTAINER_MODE)
    option(algorithms_ENABLE_IPO "Enable IPO/LTO" OFF)
    option(algorithms_WARNINGS_AS_ERRORS "Treat Warnings As Errors" OFF)
    option(algorithms_ENABLE_USER_LINKER "Enable user-selected linker" OFF)
    option(algorithms_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" OFF)
    option(algorithms_ENABLE_SANITIZER_LEAK "Enable leak sanitizer" OFF)
    option(algorithms_ENABLE_SANITIZER_UNDEFINED "Enable undefined sanitizer" OFF)
    option(algorithms_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
    option(algorithms_ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
    option(algorithms_ENABLE_UNITY_BUILD "Enable unity builds" OFF)
    option(algorithms_ENABLE_CLANG_TIDY "Enable clang-tidy" OFF)
    option(algorithms_ENABLE_CPPCHECK "Enable cpp-check analysis" OFF)
    option(algorithms_ENABLE_PCH "Enable precompiled headers" OFF)
    option(algorithms_ENABLE_CACHE "Enable ccache" OFF)
  else()
    option(algorithms_ENABLE_IPO "Enable IPO/LTO" ON)
    option(algorithms_WARNINGS_AS_ERRORS "Treat Warnings As Errors" ON)
    option(algorithms_ENABLE_USER_LINKER "Enable user-selected linker" OFF)
    option(algorithms_ENABLE_SANITIZER_ADDRESS "Enable address sanitizer" ${SUPPORTS_ASAN})
    option(algorithms_ENABLE_SANITIZER_LEAK "Enable leak sanitizer" OFF)
    option(algorithms_ENABLE_SANITIZER_UNDEFINED "Enable undefined sanitizer" ${SUPPORTS_UBSAN})
    option(algorithms_ENABLE_SANITIZER_THREAD "Enable thread sanitizer" OFF)
    option(algorithms_ENABLE_SANITIZER_MEMORY "Enable memory sanitizer" OFF)
    option(algorithms_ENABLE_UNITY_BUILD "Enable unity builds" OFF)
    option(algorithms_ENABLE_CLANG_TIDY "Enable clang-tidy" ON)
    option(algorithms_ENABLE_CPPCHECK "Enable cpp-check analysis" OFF)
    option(algorithms_ENABLE_PCH "Enable precompiled headers" OFF)
    option(algorithms_ENABLE_CACHE "Enable ccache" ON)
  endif()

  if(NOT PROJECT_IS_TOP_LEVEL)
    mark_as_advanced(
      algorithms_ENABLE_IPO
      algorithms_WARNINGS_AS_ERRORS
      algorithms_ENABLE_USER_LINKER
      algorithms_ENABLE_SANITIZER_ADDRESS
      algorithms_ENABLE_SANITIZER_LEAK
      algorithms_ENABLE_SANITIZER_UNDEFINED
      algorithms_ENABLE_SANITIZER_THREAD
      algorithms_ENABLE_SANITIZER_MEMORY
      algorithms_ENABLE_UNITY_BUILD
      algorithms_ENABLE_CLANG_TIDY
      algorithms_ENABLE_CPPCHECK
      algorithms_ENABLE_COVERAGE
      algorithms_ENABLE_PCH
      algorithms_ENABLE_CACHE)
  endif()

  algorithms_check_libfuzzer_support(LIBFUZZER_SUPPORTED)
  if(LIBFUZZER_SUPPORTED AND (algorithms_ENABLE_SANITIZER_ADDRESS OR algorithms_ENABLE_SANITIZER_THREAD OR algorithms_ENABLE_SANITIZER_UNDEFINED))
    set(DEFAULT_FUZZER ON)
  else()
    set(DEFAULT_FUZZER OFF)
  endif()

  option(algorithms_BUILD_FUZZ_TESTS "Enable fuzz testing executable" ${DEFAULT_FUZZER})

endmacro()

macro(algorithms_global_options)
  add_compile_options("$<$<CONFIG:DEBUG>:-DDEBUG>")
  if(algorithms_ENABLE_IPO)
    include(cmake/InterproceduralOptimization.cmake)
    algorithms_enable_ipo()
  endif()

  algorithms_supports_sanitizers()

  if(algorithms_ENABLE_HARDENING AND algorithms_ENABLE_GLOBAL_HARDENING)
    include(cmake/Hardening.cmake)
    if(NOT SUPPORTS_UBSAN
      OR algorithms_ENABLE_SANITIZER_UNDEFINED
      OR algorithms_ENABLE_SANITIZER_ADDRESS
      OR algorithms_ENABLE_SANITIZER_THREAD
      OR algorithms_ENABLE_SANITIZER_LEAK)
      set(ENABLE_UBSAN_MINIMAL_RUNTIME FALSE)
    else()
      set(ENABLE_UBSAN_MINIMAL_RUNTIME TRUE)
    endif()
    # message("${algorithms_ENABLE_HARDENING} ${ENABLE_UBSAN_MINIMAL_RUNTIME} ${algorithms_ENABLE_SANITIZER_UNDEFINED}")
    algorithms_enable_hardening(algorithms_options ON ${ENABLE_UBSAN_MINIMAL_RUNTIME})
  endif()
endmacro()

macro(algorithms_local_options)
  if(PROJECT_IS_TOP_LEVEL)
    include(cmake/StandardProjectSettings.cmake)
  endif()

  add_library(algorithms_warnings INTERFACE)
  add_library(algorithms_options INTERFACE)

  include(cmake/CompilerWarnings.cmake)
  algorithms_set_project_warnings(
    algorithms_warnings
    ${algorithms_WARNINGS_AS_ERRORS}
    ""
    ""
    ""
    "")

  if(algorithms_ENABLE_USER_LINKER)
    include(cmake/Linker.cmake)
    configure_linker(algorithms_options)
  endif()

  include(cmake/Sanitizers.cmake)
  algorithms_enable_sanitizers(
    algorithms_options
    ${algorithms_ENABLE_SANITIZER_ADDRESS}
    ${algorithms_ENABLE_SANITIZER_LEAK}
    ${algorithms_ENABLE_SANITIZER_UNDEFINED}
    ${algorithms_ENABLE_SANITIZER_THREAD}
    ${algorithms_ENABLE_SANITIZER_MEMORY})

  set_target_properties(algorithms_options PROPERTIES UNITY_BUILD ${algorithms_ENABLE_UNITY_BUILD})

  if(algorithms_ENABLE_PCH)
    target_precompile_headers(
      algorithms_options
      INTERFACE
      <vector>
      <string>
      <utility>)
  endif()

  if(algorithms_ENABLE_CACHE)
    include(cmake/Cache.cmake)
    algorithms_enable_cache()
  endif()

  include(cmake/StaticAnalyzers.cmake)
  if(algorithms_ENABLE_CLANG_TIDY)
    algorithms_enable_clang_tidy(algorithms_options ${algorithms_WARNINGS_AS_ERRORS})
  endif()

  # CppCheck chokes on code
  # if(algorithms_ENABLE_CPPCHECK)
  #   algorithms_enable_cppcheck(${algorithms_WARNINGS_AS_ERRORS} "" # override cppcheck options
  #   )
  # endif()

  if(algorithms_ENABLE_COVERAGE)
    include(cmake/Tests.cmake)
    algorithms_enable_coverage(algorithms_options)
  endif()

  if(algorithms_WARNINGS_AS_ERRORS)
    check_cxx_compiler_flag("-Wl,--fatal-warnings" LINKER_FATAL_WARNINGS)
    if(LINKER_FATAL_WARNINGS)
      # This is not working consistently, so disabling for now
      # target_link_options(algorithms_options INTERFACE -Wl,--fatal-warnings)
    endif()
  endif()

  if(algorithms_ENABLE_HARDENING AND NOT algorithms_ENABLE_GLOBAL_HARDENING)
    include(cmake/Hardening.cmake)
    if(NOT SUPPORTS_UBSAN
      OR algorithms_ENABLE_SANITIZER_UNDEFINED
      OR algorithms_ENABLE_SANITIZER_ADDRESS
      OR algorithms_ENABLE_SANITIZER_THREAD
      OR algorithms_ENABLE_SANITIZER_LEAK)
      set(ENABLE_UBSAN_MINIMAL_RUNTIME FALSE)
    else()
      set(ENABLE_UBSAN_MINIMAL_RUNTIME TRUE)
    endif()
    algorithms_enable_hardening(algorithms_options OFF ${ENABLE_UBSAN_MINIMAL_RUNTIME})
  endif()

endmacro()
