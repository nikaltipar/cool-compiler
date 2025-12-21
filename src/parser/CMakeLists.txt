bison_target(CoolParser
    cool.y
    ${CMAKE_CURRENT_BINARY_DIR}/cool-parse.cpp
    DEFINES_FILE ${CMAKE_CURRENT_BINARY_DIR}/cool-parse.hpp
    COMPILE_FLAGS "-d -v -y --debug -p cool_yy"
)

add_library(cool_parser STATIC
    ${BISON_CoolParser_OUTPUTS}
)

target_include_directories(cool_parser PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_BINARY_DIR}
)

target_link_libraries(cool_parser PUBLIC
    cool_common
    cool_lexer
)
