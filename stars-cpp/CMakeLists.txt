cmake_minimum_required(VERSION 3.14)

project(stars-cpp)

find_package(SDL2 REQUIRED)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 23)

file(GLOB_RECURSE "source" CONFIGURE_DEPENDS
    src/*.cpp
)

add_executable(stars-cpp ${source})

# link dependencies
target_link_libraries(stars-cpp PRIVATE SDL2::SDL2 SDL2::SDL2main)
