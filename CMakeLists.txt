cmake_minimum_required(VERSION 3.18)
project(mcpp-iterator-facade)

if (CMAKE_SOURCE_DIR STREQUAL PROJECT_SOURCE_DIR)
    set(is_toplevel TRUE)
else ()
    set(is_toplevel FALSE)
endif ()

option(${PROJECT_NAME}_WITH_TESTS "Build tests" ${is_toplevel})

add_library(${PROJECT_NAME} INTERFACE)
target_include_directories(${PROJECT_NAME} INTERFACE
    $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
    $<INSTALL_INTERFACE:include>)
target_compile_features(${PROJECT_NAME} INTERFACE cxx_std_17)
set_target_properties(${PROJECT_NAME} PROPERTIES EXPORT_NAME mcpp::iterator-facade)
add_library(mcpp::iterator-facade ALIAS ${PROJECT_NAME})

install(TARGETS ${PROJECT_NAME} EXPORT ${PROJECT_NAME}-config)
install(EXPORT ${PROJECT_NAME}-config DESTINATION share/cmake/${PROJECT_NAME})
install(DIRECTORY include DESTINATION ".")

if (${PROJECT_NAME}_WITH_TESTS)
    add_subdirectory(tests)
endif ()