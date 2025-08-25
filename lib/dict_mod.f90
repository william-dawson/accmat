module dict_mod
    use iso_fortran_env, only: real64, int32
    use iso_c_binding, only: c_null_char
    implicit none
    
    private
    public :: dict_type
    public :: dict_create, dict_destroy, dict_has_key, dict_to_json
    public :: dict_set_string, dict_set_int32, dict_set_real64, dict_set_dict
    public :: dict_set_real64_array, dict_set_real32_array, dict_set_int32_array, dict_set_int64_array
    public :: dict_get_string, dict_get_int32, dict_get_real64, dict_get_dict
    public :: dict_from_json
    
    ! Maximum sizes - reduced to avoid stack overflow
    integer, parameter :: MAX_KEY_LEN = 64
    integer, parameter :: MAX_VALUE_LEN = 1024
    integer, parameter :: MAX_DICT_SIZE = 100
    
    ! Simple key-value pair - everything is a string
    type :: dict_entry_type
        character(len=MAX_KEY_LEN) :: key = ''
        character(len=MAX_VALUE_LEN) :: value = ''
        logical :: is_used = .false.
    end type dict_entry_type
    
    ! Simple dictionary type - just key-value string pairs
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
    
    !> Destroy dictionary (just reset)
    subroutine dict_destroy(dict)
        type(dict_type), intent(inout) :: dict
        integer :: i
        
        do i = 1, MAX_DICT_SIZE
            if (dict%entries(i)%is_used) then
                dict%entries(i)%key = ''
                dict%entries(i)%value = ''
                dict%entries(i)%is_used = .false.
            end if
        end do
        dict%size = 0
    end subroutine dict_destroy
    
    !> Check if dictionary contains a key
    function dict_has_key(dict, key) result(found)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical :: found
        found = find_key_index(dict, key) > 0
    end function dict_has_key
    
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
    
    !> Find free entry index (returns 0 if full)
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
    
    !> Set string value
    subroutine dict_set_string(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key, value
        integer :: index
        
        index = find_key_index(dict, key)
        if (index == 0) then
            index = find_free_index(dict)
            if (index == 0) return  ! Dictionary full
            dict%size = dict%size + 1
            dict%entries(index)%key = trim(key)
            dict%entries(index)%is_used = .true.
        end if
        
        dict%entries(index)%value = trim(value)
    end subroutine dict_set_string
    
    !> Set int32 value (convert to string)
    subroutine dict_set_int32(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value
        character(len=32) :: value_str
        
        write(value_str, '(I0)') value
        call dict_set_string(dict, key, trim(value_str))
    end subroutine dict_set_int32
    
    !> Set real64 value (convert to string)
    subroutine dict_set_real64(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value
        character(len=32) :: value_str
        
        write(value_str, '(F0.6)') value
        call dict_set_string(dict, key, trim(value_str))
    end subroutine dict_set_real64
    
    !> Set dict value (convert to JSON string)
    subroutine dict_set_dict(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        type(dict_type), intent(in) :: value
        character(len=:), allocatable :: json_str
        
        json_str = dict_to_json(value)
        call dict_set_string(dict, key, json_str)
    end subroutine dict_set_dict
    
    !> Set real64 array as JSON string
    subroutine dict_set_real64_array(dict, key, array)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        
        json_str = array_to_json_real64(array)
        call dict_set_string(dict, key, json_str)
    end subroutine dict_set_real64_array
    
    !> Set real32 array as JSON string  
    subroutine dict_set_real32_array(dict, key, array)
        use iso_fortran_env, only: real32
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real32), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        
        json_str = array_to_json_real32(array)
        call dict_set_string(dict, key, json_str)
    end subroutine dict_set_real32_array
    
    !> Set int32 array as JSON string
    subroutine dict_set_int32_array(dict, key, array)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        
        json_str = array_to_json_int32(array)
        call dict_set_string(dict, key, json_str)
    end subroutine dict_set_int32_array
    
    !> Set int64 array as JSON string
    subroutine dict_set_int64_array(dict, key, array)
        use iso_fortran_env, only: int64
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        
        json_str = array_to_json_int64(array)
        call dict_set_string(dict, key, json_str)
    end subroutine dict_set_int64_array
    
    !> Get string value
    function dict_get_string(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        character(len=MAX_VALUE_LEN) :: value
        integer :: index
        
        value = ''
        if (present(found)) found = .false.
        
        index = find_key_index(dict, key)
        if (index > 0) then
            value = dict%entries(index)%value
            if (present(found)) found = .true.
        end if
    end function dict_get_string
    
    !> Get int32 value (parse from string)
    function dict_get_int32(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        integer(int32) :: value
        character(len=MAX_VALUE_LEN) :: str_value
        integer :: iostat
        
        value = 0
        str_value = dict_get_string(dict, key, found)
        
        if (present(found) .and. found) then
            read(str_value, '(I10)', iostat=iostat) value
            if (iostat /= 0) then
                value = 0
                if (present(found)) found = .false.
            end if
        end if
    end function dict_get_int32
    
    !> Get real64 value (parse from string)
    function dict_get_real64(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        real(real64) :: value
        character(len=MAX_VALUE_LEN) :: str_value
        integer :: iostat
        
        value = 0.0_real64
        str_value = dict_get_string(dict, key, found)
        
        if (present(found) .and. found) then
            read(str_value, '(F20.10)', iostat=iostat) value
            if (iostat /= 0) then
                value = 0.0_real64
                if (present(found)) found = .false.
            end if
        end if
    end function dict_get_real64
    
    !> Get dict value (parse JSON string into new dictionary)
    function dict_get_dict(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        type(dict_type) :: value
        character(len=MAX_VALUE_LEN) :: json_str
        
        value = dict_create()
        json_str = dict_get_string(dict, key, found)
        
        if (present(found) .and. found) then
            value = dict_from_json(json_str)
        end if
    end function dict_get_dict
    
    !> Convert dictionary to JSON string
    function dict_to_json(dict) result(json_str)
        type(dict_type), intent(in) :: dict
        character(len=:), allocatable :: json_str
        character(len=MAX_VALUE_LEN) :: temp_str
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
                
                ! Add value - check if it's already JSON (starts with { or [)
                if (dict%entries(i)%value(1:1) == '{' .or. dict%entries(i)%value(1:1) == '[') then
                    ! Raw JSON - don't quote
                    temp_str(pos:pos+len_trim(dict%entries(i)%value)-1) = trim(dict%entries(i)%value)
                    pos = pos + len_trim(dict%entries(i)%value)
                else
                    ! Regular string value - quote it
                    temp_str(pos:pos) = '"'
                    pos = pos + 1
                    temp_str(pos:pos+len_trim(dict%entries(i)%value)-1) = trim(dict%entries(i)%value)
                    pos = pos + len_trim(dict%entries(i)%value)
                    temp_str(pos:pos) = '"'
                    pos = pos + 1
                end if
            end if
        end do
        
        temp_str(pos:pos) = '}'
        pos = pos + 1
        
        json_str = temp_str(1:pos-1)
    end function dict_to_json
    
    !> Parse JSON string into dictionary (one level deep)
    !> 
    !> This function converts a JSON string into a dictionary where all values
    !> are stored as strings. Nested objects are stored as JSON strings and can
    !> be parsed separately by calling this function recursively.
    function dict_from_json(json_str) result(dict)
        character(len=*), intent(in) :: json_str
        type(dict_type) :: dict
        integer :: pos, len_str
        
        dict = dict_create()
        len_str = len_trim(json_str)
        
        ! Must start with opening brace
        if (len_str == 0 .or. json_str(1:1) /= '{') return
        
        pos = 2  ! Skip opening brace
        
        ! Parse key-value pairs
        do while (pos < len_str)
            call skip_whitespace(json_str, pos, len_str)
            
            ! Check for end of object or comma separator
            if (pos > len_str .or. json_str(pos:pos) == '}') exit
            if (json_str(pos:pos) == ',') then
                pos = pos + 1
                cycle
            end if
            
            ! Parse one key-value pair
            call parse_key_value_pair(dict, json_str, pos, len_str)
        end do
    end function dict_from_json
    
    !> Skip whitespace characters
    subroutine skip_whitespace(json_str, pos, len_str)
        character(len=*), intent(in) :: json_str
        integer, intent(inout) :: pos
        integer, intent(in) :: len_str
        
        do while (pos <= len_str)
            if (json_str(pos:pos) == ' ' .or. json_str(pos:pos) == char(9) .or. &
                json_str(pos:pos) == char(10) .or. json_str(pos:pos) == char(13)) then
                pos = pos + 1
            else
                exit
            end if
        end do
    end subroutine skip_whitespace
    
    !> Parse a single key-value pair from JSON
    subroutine parse_key_value_pair(dict, json_str, pos, len_str)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: json_str
        integer, intent(inout) :: pos
        integer, intent(in) :: len_str
        character(len=MAX_KEY_LEN) :: key
        character(len=MAX_VALUE_LEN) :: value
        
        ! Parse key
        key = parse_key_string(json_str, pos, len_str)
        if (len_trim(key) == 0) return
        
        ! Skip whitespace and colon
        call skip_whitespace(json_str, pos, len_str)
        if (pos <= len_str .and. json_str(pos:pos) == ':') then
            pos = pos + 1
            call skip_whitespace(json_str, pos, len_str)
        end if
        
        ! Parse value
        if (pos <= len_str) then
            if (json_str(pos:pos) == '"') then
                ! String value
                value = parse_string_value(json_str, pos, len_str)
            else if (json_str(pos:pos) == '{') then
                ! Nested object - extract entire object as string
                value = parse_object_value(json_str, pos, len_str)
            else
                ! Number or literal value
                value = parse_literal_value(json_str, pos, len_str)
            end if
            
            call dict_set_string(dict, trim(key), trim(value))
        end if
    end subroutine parse_key_value_pair
    
    !> Parse a quoted key string (limited to MAX_KEY_LEN)
    function parse_key_string(json_str, pos, len_str) result(key)
        character(len=*), intent(in) :: json_str
        integer, intent(inout) :: pos
        integer, intent(in) :: len_str
        character(len=MAX_KEY_LEN) :: key
        integer :: start_pos
        
        key = ''
        
        ! Must start with quote
        if (pos > len_str .or. json_str(pos:pos) /= '"') return
        
        pos = pos + 1  ! Skip opening quote
        start_pos = pos
        
        ! Find closing quote (simple - no escape handling for now)
        do while (pos <= len_str .and. json_str(pos:pos) /= '"')
            pos = pos + 1
        end do
        
        if (pos <= len_str) then
            key = json_str(start_pos:pos-1)
            pos = pos + 1  ! Skip closing quote
        end if
    end function parse_key_string

    !> Parse a quoted string value
    function parse_string_value(json_str, pos, len_str) result(value)
        character(len=*), intent(in) :: json_str
        integer, intent(inout) :: pos
        integer, intent(in) :: len_str
        character(len=MAX_VALUE_LEN) :: value
        integer :: start_pos
        
        value = ''
        
        ! Must start with quote
        if (pos > len_str .or. json_str(pos:pos) /= '"') return
        
        pos = pos + 1  ! Skip opening quote
        start_pos = pos
        
        ! Find closing quote (simple - no escape handling for now)
        do while (pos <= len_str .and. json_str(pos:pos) /= '"')
            pos = pos + 1
        end do
        
        if (pos <= len_str) then
            value = json_str(start_pos:pos-1)
            pos = pos + 1  ! Skip closing quote
        end if
    end function parse_string_value
    
    !> Parse an object value (extract as JSON string)
    function parse_object_value(json_str, pos, len_str) result(value)
        character(len=*), intent(in) :: json_str
        integer, intent(inout) :: pos
        integer, intent(in) :: len_str
        character(len=MAX_VALUE_LEN) :: value
        integer :: start_pos, brace_count
        logical :: in_string
        
        value = ''
        
        ! Must start with opening brace
        if (pos > len_str .or. json_str(pos:pos) /= '{') return
        
        start_pos = pos
        brace_count = 1
        pos = pos + 1
        in_string = .false.
        
        ! Find matching closing brace
        do while (pos <= len_str .and. brace_count > 0)
            if (.not. in_string) then
                if (json_str(pos:pos) == '{') then
                    brace_count = brace_count + 1
                else if (json_str(pos:pos) == '}') then
                    brace_count = brace_count - 1
                else if (json_str(pos:pos) == '"') then
                    in_string = .true.
                end if
            else
                if (json_str(pos:pos) == '"') then
                    ! Simple - assume no escaped quotes in nested objects
                    in_string = .false.
                end if
            end if
            pos = pos + 1
        end do
        
        if (brace_count == 0) then
            value = json_str(start_pos:pos-1)
        end if
    end function parse_object_value
    
    !> Parse a literal value (number, boolean, null)
    function parse_literal_value(json_str, pos, len_str) result(value)
        character(len=*), intent(in) :: json_str
        integer, intent(inout) :: pos
        integer, intent(in) :: len_str
        character(len=MAX_VALUE_LEN) :: value
        integer :: start_pos
        
        value = ''
        start_pos = pos
        
        ! Read until comma, brace, or whitespace
        do while (pos <= len_str)
            if (json_str(pos:pos) == ',' .or. json_str(pos:pos) == '}' .or. &
                json_str(pos:pos) == ' ' .or. json_str(pos:pos) == char(9) .or. &
                json_str(pos:pos) == char(10) .or. json_str(pos:pos) == char(13)) then
                exit
            end if
            pos = pos + 1
        end do
        
        if (pos > start_pos) then
            value = json_str(start_pos:pos-1)
        end if
    end function parse_literal_value
    
    !> Convert real64 array to JSON string
    function array_to_json_real64(array) result(json_str)
        real(real64), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        character(len=32) :: val_str
        integer :: i, n
        
        n = size(array)
        json_str = '['
        do i = 1, n
            write(val_str, '(ES15.8E2)') array(i)
            if (i > 1) json_str = json_str // ','
            json_str = json_str // trim(adjustl(val_str))
        end do
        json_str = json_str // ']'
    end function array_to_json_real64
    
    !> Convert real32 array to JSON string  
    function array_to_json_real32(array) result(json_str)
        use iso_fortran_env, only: real32
        real(real32), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        character(len=32) :: val_str
        integer :: i, n
        
        n = size(array)
        json_str = '['
        do i = 1, n
            write(val_str, '(ES15.8E2)') array(i)
            if (i > 1) json_str = json_str // ','
            json_str = json_str // trim(adjustl(val_str))
        end do
        json_str = json_str // ']'
    end function array_to_json_real32
    
    !> Convert int32 array to JSON string
    function array_to_json_int32(array) result(json_str)
        integer(int32), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        character(len=16) :: val_str
        integer :: i, n
        
        n = size(array)
        json_str = '['
        do i = 1, n
            write(val_str, '(I0)') array(i)
            if (i > 1) json_str = json_str // ','
            json_str = json_str // trim(adjustl(val_str))
        end do
        json_str = json_str // ']'
    end function array_to_json_int32
    
    !> Convert int64 array to JSON string
    function array_to_json_int64(array) result(json_str)
        use iso_fortran_env, only: int64
        integer(int64), intent(in) :: array(:)
        character(len=:), allocatable :: json_str
        character(len=24) :: val_str
        integer :: i, n
        
        n = size(array)
        json_str = '['
        do i = 1, n
            write(val_str, '(I0)') array(i)
            if (i > 1) json_str = json_str // ','
            json_str = json_str // trim(adjustl(val_str))
        end do
        json_str = json_str // ']'
    end function array_to_json_int64

end module dict_mod