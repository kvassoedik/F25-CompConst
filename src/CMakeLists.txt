cmake_minimum_required(VERSION 3.8)

project(silcc VERSION 0.0.1 LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

add_executable(${PROJECT_NAME} main.cpp)

set(LIB_NAME ${PROJECT_NAME}_lib)
add_library(${LIB_NAME}
    ${CMAKE_SOURCE_DIR}/lexer/Lexer.cpp
    ${CMAKE_SOURCE_DIR}/lexer/Token.cpp
    ${CMAKE_SOURCE_DIR}/parser/Parser.cpp
)

target_include_directories(${LIB_NAME} PUBLIC ${CMAKE_SOURCE_DIR}/include)
target_link_libraries(${PROJECT_NAME} PUBLIC ${LIB_NAME})