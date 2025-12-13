add_library(cool_cgen STATIC
    cgen.cc
    cgen_supp.cc
)

target_include_directories(cool_cgen PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(cool_cgen PUBLIC
    cool_common
    cool_semant
)

