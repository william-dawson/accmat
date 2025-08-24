cmake_minimum_required(VERSION 3.12)
project(fbuf LANGUAGES Fortran)

set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -std=f2008 -Wall -Wextra -g -O2 -fopenacc")

find_package(OpenACC QUIET)
if(OpenACC_FOUND)
    message(STATUS "OpenACC found")
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod)
file(MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY})
include_directories(${CMAKE_Fortran_MODULE_DIRECTORY})

file(GLOB LIB_SOURCES "lib/*.f90")
file(GLOB API_SOURCES "api/*.f90")
set(ALL_SOURCES ${LIB_SOURCES} ${API_SOURCES})

if(LIB_SOURCES OR API_SOURCES)
    add_library(fbuf STATIC ${ALL_SOURCES})
    set_target_properties(fbuf PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
    )
    target_include_directories(fbuf PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
endif()

# Add examples subdirectory
add_subdirectory(example)