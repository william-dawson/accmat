cmake_minimum_required(VERSION 3.12)

# Create dot_product example
add_executable(dot_product dot_product.f90)
target_link_libraries(dot_product fbuf)
target_include_directories(dot_product PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})

# Create vector_add example
add_executable(vector_add vector_add.f90)
target_link_libraries(vector_add fbuf)
target_include_directories(vector_add PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})

# Create dict_operators example
add_executable(dict_operators dict_operators.f90)
target_link_libraries(dict_operators fbuf)
target_include_directories(dict_operators PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
