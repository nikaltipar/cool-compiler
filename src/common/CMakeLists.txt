add_library(cool_common STATIC
    utilities.cpp
    stringtab.cpp
    tree.cpp
    cool-tree.cpp
    dumptype.cpp
)

target_include_directories(cool_common PUBLIC
    ${PROJECT_SOURCE_DIR}/include
    ${PROJECT_SOURCE_DIR}/src/semant
    ${PROJECT_SOURCE_DIR}/src/cgen
)
