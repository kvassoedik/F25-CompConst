cmake_minimum_required(VERSION 3.8)

project(silcc VERSION 0.0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

### Packages
find_package(LLVM REQUIRED)

### Project
add_executable(${PROJECT_NAME} main.cpp)

set(LIB_NAME ${PROJECT_NAME}_lib)
add_library(${LIB_NAME}
    ${CMAKE_SOURCE_DIR}/lexer/Lexer.cpp
    ${CMAKE_SOURCE_DIR}/lexer/Token.cpp
    ${CMAKE_SOURCE_DIR}/parser/Ast.cpp
    ${CMAKE_SOURCE_DIR}/parser/Parser.cpp
    ${CMAKE_SOURCE_DIR}/parser/srlz.cpp
    ${CMAKE_SOURCE_DIR}/parser/DebugTree.cpp
    ${CMAKE_SOURCE_DIR}/analyzer/Analyzer.cpp
    ${CMAKE_SOURCE_DIR}/analyzer/Optimizer.cpp
    ${CMAKE_SOURCE_DIR}/analyzer/utils.cpp
    ${CMAKE_SOURCE_DIR}/codegen/Codegen.cpp
)

target_link_libraries(${LIB_NAME} llvm-core::llvm-core)

target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_SOURCE_DIR}/)
target_link_libraries(${PROJECT_NAME} PUBLIC ${LIB_NAME})