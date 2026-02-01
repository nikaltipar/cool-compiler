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


add_executable(parser_test
    parser_test.cpp
)

target_link_libraries(parser_test
    PRIVATE
        GTest::gtest
        GTest::gtest_main
        -Wl,--start-group
        cool_lexer
        cool_parser
        cool_common
        cool_semant
        cool_cgen
        -Wl,--end-group
)

target_include_directories(parser_test
    PRIVATE
        ${CMAKE_SOURCE_DIR}/include
        ${CMAKE_BINARY_DIR}/src/lexer
        ${CMAKE_BINARY_DIR}/src/parser
)

target_compile_definitions(parser_test
    PRIVATE
        TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/parser_files"
)

add_executable(semant_test
    semant_test.cpp
)

target_link_libraries(semant_test
    PRIVATE
        GTest::gtest
        GTest::gtest_main
        -Wl,--start-group
        cool_lexer
        cool_parser
        cool_common
        cool_semant
        cool_cgen
        -Wl,--end-group
)

target_include_directories(semant_test
    PRIVATE
        ${CMAKE_SOURCE_DIR}/include
        ${CMAKE_BINARY_DIR}/src/lexer
        ${CMAKE_BINARY_DIR}/src/parser
        ${CMAKE_BINARY_DIR}/src/semant
)

target_compile_definitions(semant_test
    PRIVATE
        TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/semant_files"
)

add_executable(cgen_test
    cgen_test.cpp
)

target_link_libraries(cgen_test
    PRIVATE
        GTest::gtest
        GTest::gtest_main
        -Wl,--start-group
        cool_lexer
        cool_parser
        cool_common
        cool_semant
        cool_cgen
        -Wl,--end-group
)

target_include_directories(cgen_test
    PRIVATE
        ${CMAKE_SOURCE_DIR}/include
        ${CMAKE_BINARY_DIR}/src/lexer
        ${CMAKE_BINARY_DIR}/src/parser
        ${CMAKE_BINARY_DIR}/src/semant
        ${CMAKE_BINARY_DIR}/src/cgen
)

target_compile_definitions(cgen_test
    PRIVATE
        TEST_DATA_DIR="${CMAKE_CURRENT_SOURCE_DIR}/cgen_files"
)

include(GoogleTest)
gtest_discover_tests(lexer_test)
gtest_discover_tests(parser_test)
gtest_discover_tests(semant_test)

# TODO: add cgen tests for Windows.
# Might need to build spim from source for Windows.
IF(LINUX)
    gtest_discover_tests(cgen_test)
endif()