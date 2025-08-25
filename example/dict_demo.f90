program dict_demo
    use dict, only: dict_type, create, destroy, has_key, dict_set, dict_get, print_json, to_json, from_json
    use molds
    use iso_fortran_env, only: real64, real32, int32, int64
    implicit none
    
    type(dict_type) :: my_dict, nested_dict, parsed_dict
    character(len=:), allocatable :: json_str
    character(len=1024) :: str_val
    integer(int32) :: int_val
    logical :: found
    
    ! Test arrays for demo
    real(real64) :: real64_data(3) = [1.1_real64, 2.2_real64, 3.3_real64]
    real(real32) :: real32_data(2) = [1.5_real32, 2.5_real32]
    integer(int32) :: int32_data(4) = [10_int32, 20_int32, 30_int32, 40_int32]
    integer(int64) :: int64_data(2) = [100000_int64, 200000_int64]
    
    write(*,*) 'Dictionary Demo'
    write(*,*) ''
    
    ! Basic operations
    write(*,*) '1. Basic operations'
    my_dict = create()
    call dict_set(my_dict, 'name', 'Alice')
    call dict_set(my_dict, 'age', 30_int32)
    call dict_set(my_dict, 'score', 85.5_real64)
    
    str_val = dict_get(my_dict, 'name', string_mold, found)
    write(*,*) 'name =', '"' // trim(str_val) // '"'
    
    int_val = dict_get(my_dict, 'age', int32_mold, found)
    write(*,*) 'age =', int_val
    
    write(*,*) 'has_key("name") =', has_key(my_dict, 'name')
    
    write(*,*) 'JSON:'
    call print_json(my_dict)
    write(*,*) ''
    
    ! Arrays
    write(*,*) '2. Typed arrays (automatic JSON conversion)'
    call destroy(my_dict)
    my_dict = create()
    
    call dict_set(my_dict, 'real64_array', real64_data)
    call dict_set(my_dict, 'real32_array', real32_data)  
    call dict_set(my_dict, 'int32_array', int32_data)
    call dict_set(my_dict, 'int64_array', int64_data)
    
    str_val = dict_get(my_dict, 'real64_array', string_mold, found)
    write(*,*) 'real64_array =', trim(str_val)
    
    str_val = dict_get(my_dict, 'int32_array', string_mold, found)
    write(*,*) 'int32_array =', trim(str_val)
    
    write(*,*) 'Arrays JSON:'
    call print_json(my_dict)
    write(*,*) ''
    
    ! Complex structures
    write(*,*) '3. Complex nested structures'
    call destroy(my_dict)
    my_dict = create()
    
    call dict_set(my_dict, 'users', '[{"name":"Alice","dept":"Engineering"},{"name":"Bob","dept":"Sales"}]')
    call dict_set(my_dict, 'config', '{"server":"prod","ports":[8080,443],"active":true}')
    
    str_val = dict_get(my_dict, 'users', string_mold, found)
    write(*,*) 'users =', trim(str_val)
    
    str_val = dict_get(my_dict, 'config', string_mold, found)
    write(*,*) 'config =', trim(str_val)
    write(*,*) ''
    
    ! Nested dictionaries
    write(*,*) '4. Nested dictionaries'
    nested_dict = create()
    call dict_set(nested_dict, 'street', '123 Main St')
    call dict_set(nested_dict, 'city', 'Boston')
    call dict_set(nested_dict, 'zip', 12345_int32)
    
    call dict_set(my_dict, 'address', nested_dict)
    call dict_set(my_dict, 'active', 'true')
    
    ! Access nested dictionary directly
    parsed_dict = dict_get(my_dict, 'address', dict_mold, found)
    if (found) then
        str_val = dict_get(parsed_dict, 'city', string_mold, found)
        write(*,*) 'address.city =', '"' // trim(str_val) // '"'
        call destroy(parsed_dict)
    end if
    
    write(*,*) 'Nested JSON:'
    call print_json(my_dict)
    json_str = to_json(my_dict)  ! Keep for parsing demo
    write(*,*) ''
    
    ! JSON parsing
    write(*,*) '5. JSON parsing'
    parsed_dict = from_json(json_str)
    
    str_val = dict_get(parsed_dict, 'active', string_mold, found)
    write(*,*) 'parsed active =', '"' // trim(str_val) // '"'
    
    str_val = dict_get(parsed_dict, 'address', string_mold, found)
    write(*,*) 'parsed address =', trim(str_val)
    write(*,*) ''
    
    ! Real-world example
    write(*,*) '6. Real-world configuration'
    call destroy(my_dict)
    my_dict = create()
    
    call dict_set(my_dict, 'database', '{"host":"db.example.com","port":5432,"replicas":["db1","db2"]}')
    call dict_set(my_dict, 'logging', '{"level":"INFO","outputs":["console","file"]}')
    
    str_val = dict_get(my_dict, 'database', string_mold, found)
    write(*,*) 'database =', trim(str_val)
    
    str_val = dict_get(my_dict, 'logging', string_mold, found)
    write(*,*) 'logging =', trim(str_val)
    write(*,*) ''
    
    ! Memory cleanup
    call destroy(my_dict)
    call destroy(nested_dict)
    call destroy(parsed_dict)
    
    write(*,*) 'Demo complete - all dictionaries destroyed'

end program dict_demo