add_library(cool_common STATIC
    utilities.cc
    stringtab.cc
    tree.cc
    cool-tree.cc
    dumptype.cc
)

target_include_directories(cool_common PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/src/semant
    ${PROJECT_SOURCE_DIR}/src/cgen
)
