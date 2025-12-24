add_executable(coolc
    main.cpp
)

target_include_directories(coolc PRIVATE
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/src/semant
    ${PROJECT_SOURCE_DIR}/src/cgen
)

target_link_libraries(coolc PRIVATE
    cool_lexer
    cool_parser
    CLI11::CLI11
)

# Handle circular dependencies between common/semant/cgen on Linux
# TODO better way to handle this.
if(UNIX)
    target_link_libraries(coolc PRIVATE
        -Wl,--start-group
        cool_common
        cool_semant
        cool_cgen
        -Wl,--end-group
    )
else()
    target_link_libraries(coolc PRIVATE
        cool_common
        cool_semant
        cool_cgen
    )
endif()
