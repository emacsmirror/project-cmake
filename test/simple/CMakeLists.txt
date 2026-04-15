cmake_minimum_required(VERSION 3.25)

project(libmandala LANGUAGES CXX)

set(CMAKE_EXPORT_COMPILE_COMMANDS YES)

add_executable(test
  test.cpp
)
