cmake_minimum_required(VERSION 3.12)
project(fbuf LANGUAGES Fortran)

# Enable testing
enable_testing()

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -Wall -Wextra -g -O2 -fopenacc")

find_package(OpenACC QUIET)
if(OpenACC_FOUND)
    message(STATUS "OpenACC found")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

# Add subdirectories for lib and api
add_subdirectory(lib)
add_subdirectory(api)

# Create final library from object libraries
add_library(fbuf STATIC $<TARGET_OBJECTS:fbuf_impl> $<TARGET_OBJECTS:fbuf_api>)
set_target_properties(fbuf PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
)
target_include_directories(fbuf PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})

# Add examples subdirectory
add_subdirectory(example)