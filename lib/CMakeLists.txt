# Library implementation sources
file(GLOB LIB_SOURCES "*.f90")

if(LIB_SOURCES)
    add_library(fbuf_impl OBJECT ${LIB_SOURCES})
    set_target_properties(fbuf_impl PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
    )
    target_include_directories(fbuf_impl PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
endif()