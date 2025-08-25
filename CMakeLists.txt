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

# Documentation with FORD
find_program(FORD_EXECUTABLE ford)
if(FORD_EXECUTABLE)
    message(STATUS "FORD found: ${FORD_EXECUTABLE}")
    
    # Documentation target
    add_custom_target(docs
        COMMAND ${FORD_EXECUTABLE} ford.md
        WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
        COMMENT "Generating API documentation with FORD"
        VERBATIM
    )
    
    # Make docs depend on source files so it rebuilds when needed
    file(GLOB_RECURSE API_SOURCES "api/*.f90")
    add_custom_target(docs_check
        DEPENDS ${API_SOURCES} ${CMAKE_SOURCE_DIR}/README.md ${CMAKE_SOURCE_DIR}/ford.md
    )
    add_dependencies(docs docs_check)
    
    # Optional: Add install target for docs
    install(DIRECTORY ${CMAKE_SOURCE_DIR}/docs/
            DESTINATION share/doc/fbuf
            OPTIONAL
            PATTERN "*.html"
            PATTERN "*.css" 
            PATTERN "*.js"
            PATTERN "*.png"
            PATTERN "*.json"
    )
    
    message(STATUS "Documentation target 'docs' available - run 'make docs' or 'cmake --build . --target docs'")
else()
    message(STATUS "FORD not found - documentation generation disabled")
    message(STATUS "Install FORD with: pip install ford")
endif()