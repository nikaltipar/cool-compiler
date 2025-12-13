flex_target(CoolLexer
    cool.flex
    ${CMAKE_CURRENT_BINARY_DIR}/cool-lex.cc
    DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/cool-lex.h
)

# Lexer as a static library
add_library(cool_lexer STATIC
    ${FLEX_CoolLexer_OUTPUTS}
)

target_include_directories(cool_lexer PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries(cool_lexer PUBLIC
    cool_common
)
