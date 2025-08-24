program dict_operators
    use dict
    use molds
    use iso_fortran_env, only: real64, int32
    implicit none
    
    type(dict_type) :: my_dict
    real(real64) :: test_array(5) = [1.1_real64, 2.2_real64, 3.3_real64, 4.4_real64, 5.5_real64]
    character(len=:), allocatable :: json_output
    
    integer(int32) :: int_val
    real(real64) :: real_val
    character(len=1024) :: str_val
    logical :: found
    
    write(*,*) 'Dictionary Operators Demo'
    write(*,*) '========================='
    
    ! Create dictionary
    my_dict = create()
    
    ! Set values using generic dict_set
    call dict_set(my_dict, 'name', 'Fortran Dictionary')
    call dict_set(my_dict, 'version', 2_int32)
    call dict_set(my_dict, 'pi', 3.14159_real64)
    call dict_set(my_dict, 'data', test_array)
    
    write(*,*) 'Set values using generic dict_set interface'
    
    ! Get values using mold pattern
    str_val = dict_get(my_dict, 'name', string_mold, found)
    if (found) then
        write(*,*) 'name =', '"' // trim(str_val) // '"'
    end if
    
    int_val = dict_get(my_dict, 'version', int32_mold, found)
    if (found) then
        write(*,*) 'version =', int_val
    end if
    
    real_val = dict_get(my_dict, 'pi', real64_mold, found)
    if (found) then
        write(*,*) 'pi =', real_val
    end if
    
    ! Check if keys exist
    write(*,*) 'Has key "name":', has_key(my_dict, 'name')
    write(*,*) 'Has key "missing":', has_key(my_dict, 'missing')
    
    ! Demonstrate updating existing values
    write(*,*) ''
    write(*,*) 'Updating existing values...'
    call dict_set(my_dict, 'version', 3_int32)
    call dict_set(my_dict, 'pi', 3.141592653589793_real64)
    
    int_val = dict_get(my_dict, 'version', int32_mold, found)
    if (found) then
        write(*,*) 'updated version =', int_val
    end if
    
    real_val = dict_get(my_dict, 'pi', real64_mold, found)
    if (found) then
        write(*,*) 'updated pi =', real_val
    end if
    
    ! Output as JSON
    write(*,*) ''
    write(*,*) 'JSON output:'
    json_output = to_json(my_dict)
    write(*,*) json_output
    
    ! Clean up
    call destroy(my_dict)
    
    write(*,*) ''
    write(*,*) 'Dictionary destroyed successfully'

end program dict_operators