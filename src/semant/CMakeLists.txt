# Cool Semantic Analyzer library

add_library(cool_semant STATIC
    semant.cc
)

target_include_directories(cool_semant PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(cool_semant PUBLIC
    cool_common
    cool_parser
)

