module dict_mod
    use iso_fortran_env, only: real32, real64, int32, int64
    use iso_c_binding, only: c_null_char
    implicit none
    
    private
    public :: dict_type, dict_value_type
    public :: dict_create, dict_destroy, dict_has_key, dict_to_json
    public :: dict_set_int32, dict_set_real64, dict_set_string, dict_set_real64_array
    public :: dict_get_int32, dict_get_real64, dict_get_string
    public :: DICT_INT32, DICT_INT64, DICT_REAL32, DICT_REAL64, DICT_STRING, DICT_ARRAY_INT32, DICT_ARRAY_REAL64
    
    ! Data type constants
    integer, parameter :: DICT_INVALID = 0
    integer, parameter :: DICT_INT32 = 1
    integer, parameter :: DICT_INT64 = 2
    integer, parameter :: DICT_REAL32 = 3
    integer, parameter :: DICT_REAL64 = 4
    integer, parameter :: DICT_STRING = 5
    integer, parameter :: DICT_ARRAY_INT32 = 6
    integer, parameter :: DICT_ARRAY_REAL64 = 7
    
    ! Maximum string length and key length
    integer, parameter :: MAX_STRING_LEN = 1024
    integer, parameter :: MAX_KEY_LEN = 256
    integer, parameter :: MAX_DICT_SIZE = 1000
    
    ! Value type for storing different data types
    type :: dict_value_type
        integer :: value_type = DICT_INVALID
        
        ! Scalar values
        integer(int32) :: int32_val
        integer(int64) :: int64_val
        real(real32) :: real32_val
        real(real64) :: real64_val
        character(len=MAX_STRING_LEN) :: string_val
        
        ! Array values (simplified - fixed size for now)
        integer :: array_size = 0
        integer(int32), allocatable :: int32_array(:)
        real(real64), allocatable :: real64_array(:)
    end type dict_value_type
    
    ! Key-value pair
    type :: dict_entry_type
        character(len=MAX_KEY_LEN) :: key = ''
        type(dict_value_type) :: value
        logical :: is_used = .false.
    end type dict_entry_type
    
    ! Dictionary type
    type :: dict_type
        type(dict_entry_type) :: entries(MAX_DICT_SIZE)
        integer :: size = 0
    end type dict_type

contains

    !> Create a new dictionary
    function dict_create() result(dict)
        type(dict_type) :: dict
        dict%size = 0
        ! entries are automatically initialized
    end function dict_create
    
    !> Destroy dictionary and cleanup memory
    subroutine dict_destroy(dict)
        type(dict_type), intent(inout) :: dict
        integer :: i
        
        do i = 1, MAX_DICT_SIZE
            if (dict%entries(i)%is_used) then
                ! Cleanup allocated arrays
                if (allocated(dict%entries(i)%value%int32_array)) then
                    deallocate(dict%entries(i)%value%int32_array)
                end if
                if (allocated(dict%entries(i)%value%real64_array)) then
                    deallocate(dict%entries(i)%value%real64_array)
                end if
                dict%entries(i)%is_used = .false.
            end if
        end do
        
        dict%size = 0
    end subroutine dict_destroy
    
    !> Find entry index for a key (returns 0 if not found)
    function find_key_index(dict, key) result(index)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        integer :: index
        integer :: i
        
        index = 0
        do i = 1, MAX_DICT_SIZE
            if (dict%entries(i)%is_used .and. trim(dict%entries(i)%key) == trim(key)) then
                index = i
                return
            end if
        end do
    end function find_key_index
    
    !> Find next available entry index
    function find_free_index(dict) result(index)
        type(dict_type), intent(in) :: dict
        integer :: index
        integer :: i
        
        index = 0
        do i = 1, MAX_DICT_SIZE
            if (.not. dict%entries(i)%is_used) then
                index = i
                return
            end if
        end do
    end function find_free_index
    
    !> Check if dictionary has a key
    function dict_has_key(dict, key) result(has_key)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical :: has_key
        
        has_key = find_key_index(dict, key) > 0
    end function dict_has_key
    
    !> Set integer value in dictionary
    subroutine dict_set_int32(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value
        integer :: index
        
        index = find_key_index(dict, key)
        if (index == 0) then
            index = find_free_index(dict)
            if (index == 0) return  ! Dictionary full
            dict%size = dict%size + 1
            dict%entries(index)%key = trim(key)
            dict%entries(index)%is_used = .true.
        end if
        
        dict%entries(index)%value%value_type = DICT_INT32
        dict%entries(index)%value%int32_val = value
    end subroutine dict_set_int32
    
    !> Set real64 value in dictionary
    subroutine dict_set_real64(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value
        integer :: index
        
        index = find_key_index(dict, key)
        if (index == 0) then
            index = find_free_index(dict)
            if (index == 0) return  ! Dictionary full
            dict%size = dict%size + 1
            dict%entries(index)%key = trim(key)
            dict%entries(index)%is_used = .true.
        end if
        
        dict%entries(index)%value%value_type = DICT_REAL64
        dict%entries(index)%value%real64_val = value
    end subroutine dict_set_real64
    
    !> Set string value in dictionary
    subroutine dict_set_string(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        integer :: index
        
        index = find_key_index(dict, key)
        if (index == 0) then
            index = find_free_index(dict)
            if (index == 0) return  ! Dictionary full
            dict%size = dict%size + 1
            dict%entries(index)%key = trim(key)
            dict%entries(index)%is_used = .true.
        end if
        
        dict%entries(index)%value%value_type = DICT_STRING
        dict%entries(index)%value%string_val = trim(value)
    end subroutine dict_set_string
    
    !> Set real64 array value in dictionary
    subroutine dict_set_real64_array(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value(:)
        integer :: index
        
        index = find_key_index(dict, key)
        if (index == 0) then
            index = find_free_index(dict)
            if (index == 0) return  ! Dictionary full
            dict%size = dict%size + 1
            dict%entries(index)%key = trim(key)
            dict%entries(index)%is_used = .true.
        end if
        
        ! Cleanup existing array if present
        if (allocated(dict%entries(index)%value%real64_array)) then
            deallocate(dict%entries(index)%value%real64_array)
        end if
        
        dict%entries(index)%value%value_type = DICT_ARRAY_REAL64
        dict%entries(index)%value%array_size = size(value)
        allocate(dict%entries(index)%value%real64_array(size(value)))
        dict%entries(index)%value%real64_array = value
    end subroutine dict_set_real64_array
    
    !> Get integer value from dictionary
    function dict_get_int32(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        integer(int32) :: value
        integer :: index
        
        value = 0
        index = find_key_index(dict, key)
        
        if (present(found)) found = .false.
        
        if (index > 0 .and. dict%entries(index)%value%value_type == DICT_INT32) then
            value = dict%entries(index)%value%int32_val
            if (present(found)) found = .true.
        end if
    end function dict_get_int32
    
    !> Get real64 value from dictionary
    function dict_get_real64(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        real(real64) :: value
        integer :: index
        
        value = 0.0_real64
        index = find_key_index(dict, key)
        
        if (present(found)) found = .false.
        
        if (index > 0 .and. dict%entries(index)%value%value_type == DICT_REAL64) then
            value = dict%entries(index)%value%real64_val
            if (present(found)) found = .true.
        end if
    end function dict_get_real64
    
    !> Get string value from dictionary
    function dict_get_string(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        character(len=MAX_STRING_LEN) :: value
        integer :: index
        
        value = ''
        index = find_key_index(dict, key)
        
        if (present(found)) found = .false.
        
        if (index > 0 .and. dict%entries(index)%value%value_type == DICT_STRING) then
            value = dict%entries(index)%value%string_val
            if (present(found)) found = .true.
        end if
    end function dict_get_string
    
    !> Convert dictionary to JSON format
    function dict_to_json(dict) result(json_str)
        type(dict_type), intent(in) :: dict
        character(len=:), allocatable :: json_str
        character(len=10000) :: temp_str
        integer :: i, pos
        logical :: first_entry
        
        temp_str = '{'
        pos = 2
        first_entry = .true.
        
        do i = 1, MAX_DICT_SIZE
            if (dict%entries(i)%is_used) then
                ! Add comma separator for non-first entries
                if (.not. first_entry) then
                    temp_str(pos:pos) = ','
                    pos = pos + 1
                end if
                first_entry = .false.
                
                ! Add key
                temp_str(pos:pos) = '"'
                pos = pos + 1
                temp_str(pos:pos+len_trim(dict%entries(i)%key)-1) = trim(dict%entries(i)%key)
                pos = pos + len_trim(dict%entries(i)%key)
                temp_str(pos:pos+1) = '":'
                pos = pos + 2
                
                ! Add value based on type
                select case(dict%entries(i)%value%value_type)
                case(DICT_INT32)
                    call add_int_to_json(temp_str, pos, dict%entries(i)%value%int32_val)
                case(DICT_REAL64)
                    call add_real_to_json(temp_str, pos, dict%entries(i)%value%real64_val)
                case(DICT_STRING)
                    call add_string_to_json(temp_str, pos, dict%entries(i)%value%string_val)
                case(DICT_ARRAY_REAL64)
                    call add_real_array_to_json(temp_str, pos, &
                        dict%entries(i)%value%real64_array, &
                        dict%entries(i)%value%array_size)
                end select
            end if
        end do
        
        temp_str(pos:pos) = '}'
        pos = pos + 1
        
        json_str = temp_str(1:pos-1)
    end function dict_to_json
    
    !> Helper: Add integer to JSON string
    subroutine add_int_to_json(str, pos, val)
        character(len=*), intent(inout) :: str
        integer, intent(inout) :: pos
        integer(int32), intent(in) :: val
        character(len=20) :: int_str
        
        write(int_str, '(i0)') val
        str(pos:pos+len_trim(int_str)-1) = trim(int_str)
        pos = pos + len_trim(int_str)
    end subroutine add_int_to_json
    
    !> Helper: Add real to JSON string
    subroutine add_real_to_json(str, pos, val)
        character(len=*), intent(inout) :: str
        integer, intent(inout) :: pos
        real(real64), intent(in) :: val
        character(len=30) :: real_str
        
        write(real_str, '(g0)') val
        str(pos:pos+len_trim(real_str)-1) = trim(real_str)
        pos = pos + len_trim(real_str)
    end subroutine add_real_to_json
    
    !> Helper: Add string to JSON string
    subroutine add_string_to_json(str, pos, val)
        character(len=*), intent(inout) :: str
        integer, intent(inout) :: pos
        character(len=*), intent(in) :: val
        
        str(pos:pos) = '"'
        pos = pos + 1
        str(pos:pos+len_trim(val)-1) = trim(val)
        pos = pos + len_trim(val)
        str(pos:pos) = '"'
        pos = pos + 1
    end subroutine add_string_to_json
    
    !> Helper: Add real array to JSON string
    subroutine add_real_array_to_json(str, pos, arr, arr_size)
        character(len=*), intent(inout) :: str
        integer, intent(inout) :: pos
        real(real64), intent(in) :: arr(:)
        integer, intent(in) :: arr_size
        integer :: i
        character(len=30) :: real_str
        
        str(pos:pos) = '['
        pos = pos + 1
        
        do i = 1, arr_size
            if (i > 1) then
                str(pos:pos) = ','
                pos = pos + 1
            end if
            
            write(real_str, '(g0)') arr(i)
            str(pos:pos+len_trim(real_str)-1) = trim(real_str)
            pos = pos + len_trim(real_str)
        end do
        
        str(pos:pos) = ']'
        pos = pos + 1
    end subroutine add_real_array_to_json

end module dict_mod