find_package(GTest CONFIG REQUIRED)

add_executable(lexer_test
    lexer_test.cpp
)

target_link_libraries(lexer_test
    PRIVATE
        GTest::gtest
        GTest::gtest_main
        cool_lexer
        cool_common
)

target_include_directories(lexer_test
    PRIVATE
        ${CMAKE_SOURCE_DIR}/include
        ${CMAKE_BINARY_DIR}/src/lexer
)

target_compile_definitions(lexer_test
    PRIVATE
        TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/lexer_files"
)

# TODO add tests for other phases below.

include(GoogleTest)
gtest_discover_tests(lexer_test)
