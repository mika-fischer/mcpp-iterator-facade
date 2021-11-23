cmake_minimum_required(VERSION 3.18)
project(mcpp-iterator-facade)

if (CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    set(is_toplevel TRUE)
else ()
    set(is_toplevel FALSE)
endif ()

option(mcpp_iterator_facade_WITH_TESTS "Build tests" ${is_toplevel})

add_library(mcpp-iterator-facade INTERFACE)
target_include_directories(mcpp-iterator-facade INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)
target_compile_features(mcpp-iterator-facade INTERFACE cxx_std_17)
set_target_properties(mcpp-iterator-facade PROPERTIES EXPORT_NAME mcpp::iterator-facade)
add_library(mcpp::iterator-facade ALIAS mcpp-iterator-facade)

install(TARGETS mcpp-iterator-facade EXPORT ${PROJECT_NAME}-config)
install(EXPORT ${PROJECT_NAME}-config DESTINATION share/cmake/${PROJECT_NAME})
install(DIRECTORY include DESTINATION ".")

if (mcpp_iterator_facade_WITH_TESTS)
    add_subdirectory(tests)
endif ()