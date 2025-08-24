# API layer sources
file(GLOB API_SOURCES "*.f90")

if(API_SOURCES)
    add_library(fbuf_api OBJECT ${API_SOURCES})
    set_target_properties(fbuf_api PROPERTIES
        Fortran_MODULE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY}
    )
    target_include_directories(fbuf_api PUBLIC ${CMAKE_Fortran_MODULE_DIRECTORY})
    
    # API depends on implementation
    add_dependencies(fbuf_api fbuf_impl)
endif()