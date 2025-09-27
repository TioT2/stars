cmake_minimum_required(VERSION 3.14)

project(stars-c)

find_package(SDL2 REQUIRED)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 90)
set(CMAKE_C_STANDARD_REQUIRED ON)

file(GLOB_RECURSE "source" CONFIGURE_DEPENDS
    src/*.c
)

add_executable(stars-c ${source})

# link dependencies
target_link_libraries(stars-c PRIVATE SDL2::SDL2 SDL2::SDL2main)
target_link_libraries(stars-c PRIVATE m)
