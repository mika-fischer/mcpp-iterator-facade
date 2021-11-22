cmake_minimum_required(VERSION 3.18)
project(mcpp_iterator_facade)

if (CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    set(is_toplevel TRUE)
else ()
    set(is_toplevel FALSE)
endif ()

option(mcpp_iterator_facade_WITH_TESTS "Build tests" ${is_toplevel})

add_library(mcpp_iterator_facade INTERFACE)
target_include_directories(mcpp_iterator_facade INTERFACE include)
target_compile_features(mcpp_iterator_facade INTERFACE cxx_std_17)
add_library(MCPP::iterator_facade ALIAS mcpp_iterator_facade)

if (mcpp_iterator_facade_WITH_TESTS)
    add_subdirectory(tests)
endif ()