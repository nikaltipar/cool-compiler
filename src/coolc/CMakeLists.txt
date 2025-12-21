add_executable(coolc
    main.cpp
)

target_include_directories(coolc PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/src/semant
    ${PROJECT_SOURCE_DIR}/src/cgen
)

target_link_libraries(coolc PRIVATE
    cool_common
    cool_lexer
    cool_parser
    cool_semant
    cool_cgen
    CLI11::CLI11
)
