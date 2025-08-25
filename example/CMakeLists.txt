cmake_minimum_required(VERSION 3.12)

# Create dot_product example
add_executable(dot_product dot_product.f90)
target_link_libraries(dot_product fbuf)
target_include_directories(dot_product PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})

# Create vector_add example
add_executable(vector_add vector_add.f90)
target_link_libraries(vector_add fbuf)
target_include_directories(vector_add PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})

# Create unified dictionary demo
add_executable(dict_demo dict_demo.f90)
target_link_libraries(dict_demo fbuf)
target_include_directories(dict_demo PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})

# Create linear solver example
add_executable(lin_solve lin_solve.f90)
target_link_libraries(lin_solve fbuf)
target_include_directories(lin_solve PRIVATE ${CMAKE_Fortran_MODULE_DIRECTORY})
