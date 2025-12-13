cmake_minimum_required(VERSION 3.21)
project(CoolCompiler VERSION 1.0 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

if(WIN32)
    set(WINFLEXBISON_DIR "${CMAKE_SOURCE_DIR}/tools/winflexbison")
    if(EXISTS "${WINFLEXBISON_DIR}/win_flex.exe")
        set(FLEX_EXECUTABLE "${WINFLEXBISON_DIR}/win_flex.exe")
        set(FLEX_INCLUDE_DIR "${WINFLEXBISON_DIR}")
    endif()
    if(EXISTS "${WINFLEXBISON_DIR}/win_bison.exe")
        set(BISON_EXECUTABLE "${WINFLEXBISON_DIR}/win_bison.exe")
    endif()
endif()

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)
find_package(CLI11 CONFIG REQUIRED)

message(STATUS "Found Flex: ${FLEX_EXECUTABLE} (version ${FLEX_VERSION})")
message(STATUS "Found Bison: ${BISON_EXECUTABLE} (version ${BISON_VERSION})")

if(MSVC)
    add_compile_options(/W3)
    add_compile_options(/Zc:strictStrings-)
    add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
else()
    add_compile_options(-Wall -Wextra -Wno-unused-parameter -Wno-write-strings)
endif()

add_subdirectory(src/common)
add_subdirectory(src/lexer)
add_subdirectory(src/parser)
add_subdirectory(src/semant)
add_subdirectory(src/cgen)
add_subdirectory(src/coolc)

install(TARGETS coolc
    RUNTIME DESTINATION bin
)

install(DIRECTORY examples/
    DESTINATION share/cool/examples
    FILES_MATCHING PATTERN "*.cl"
)

