!> Dictionary API for storing mixed-type key-value pairs
!>
!> This module provides a clean API for creating and managing dictionaries
!> that can store integers, reals, strings, and arrays. The dictionary
!> supports JSON serialization for easy data exchange.
!>
!> Key Features:
!> ------------
!> - Generic interfaces for type-safe operations
!> - Mold pattern for unambiguous type resolution
!> - JSON export functionality
!> - Memory-safe resource management
!> - Support for basic Fortran types and arrays
!>
!> Example Usage:
!> -------------
!> ```fortran
!> use dict
!> use dict_molds
!> 
!> type(dict_type) :: my_dict
!> integer(int32) :: count
!> character(len=256) :: name
!> character(len=:), allocatable :: json_str
!> logical :: found
!> 
!> ! Create dictionary and add values
!> my_dict = create()
!> call dict_set(my_dict, 'name', 'John Doe')
!> call dict_set(my_dict, 'age', 30_int32)
!> call dict_set(my_dict, 'pi', 3.14159_real64)
!> 
!> ! Retrieve values using mold pattern
!> name = dict_get(my_dict, 'name', string_mold, found)
!> count = dict_get(my_dict, 'age', int32_mold, found)
!> 
!> ! Export to JSON
!> json_str = to_json(my_dict)
!> 
!> ! Clean up
!> call destroy(my_dict)
!> ```
module dict
    use dict_mod, only: dict_type, dict_create, dict_destroy, dict_has_key, dict_to_json, &
                        dict_set_int32, dict_set_real64, dict_set_string, dict_set_dict, &
                        dict_set_real64_array, dict_set_real32_array, dict_set_int32_array, dict_set_int64_array, &
                        dict_get_int32, dict_get_real64, dict_get_string, dict_get_dict, &
                        dict_from_json
    use molds, only: int32_mold, real64_mold, string_mold, dict_mold
    use iso_fortran_env, only: real64, int32, int64
    implicit none
    
    public :: dict_type
    public :: create, destroy, has_key, to_json, from_json
    public :: dict_set, dict_get
    
    ! Generic interfaces for setting values
    interface dict_set
        module procedure :: set_int32, set_real64, set_string, set_dict
        module procedure :: set_real64_array, set_real32_array, set_int32_array, set_int64_array
    end interface
    
    ! Generic interfaces for getting values using mold pattern
    interface dict_get
        module procedure :: get_int32_mold, get_real64_mold, get_string_mold, get_dict_mold
    end interface

contains

    !> Create a new dictionary
    !>
    !> Returns:
    !>     dict_type: Empty dictionary ready for use
    !>
    !> Example:
    !> ```fortran
    !> type(dict_type) :: my_dict
    !> my_dict = create()
    !> ```
    function create() result(dict)
        type(dict_type) :: dict
        dict = dict_create()
    end function create
    
    !> Destroy dictionary and cleanup memory
    !>
    !> Frees all allocated memory and resets the dictionary.
    !> Should be called when dictionary is no longer needed.
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to destroy
    !>
    !> Example:
    !> ```fortran
    !> call destroy(my_dict)
    !> ```
    subroutine destroy(dict)
        type(dict_type), intent(inout) :: dict
        call dict_destroy(dict)
    end subroutine destroy
    
    !> Check if dictionary contains a key
    !>
    !> Parameters:
    !>     dict : dict_type, intent(in)
    !>         Dictionary to search
    !>     key : character(len=*), intent(in)
    !>         Key to search for
    !>
    !> Returns:
    !>     logical: True if key exists, False otherwise
    !>
    !> Example:
    !> ```fortran
    !> if (has_key(my_dict, 'name')) then
    !>     print *, 'Dictionary contains name key'
    !> end if
    !> ```
    function has_key(dict, key) result(found)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical :: found
        found = dict_has_key(dict, key)
    end function has_key
    
    !> Convert dictionary to JSON string
    !>
    !> Serializes the dictionary into a JSON-formatted string suitable
    !> for data exchange or storage.
    !>
    !> Parameters:
    !>     dict : dict_type, intent(in)
    !>         Dictionary to serialize
    !>
    !> Returns:
    !>     character(len=:), allocatable: JSON representation of dictionary
    !>
    !> Example:
    !> ```fortran
    !> character(len=:), allocatable :: json_str
    !> json_str = to_json(my_dict)
    !> print *, json_str
    !> ```
    function to_json(dict) result(json_str)
        type(dict_type), intent(in) :: dict
        character(len=:), allocatable :: json_str
        json_str = dict_to_json(dict)
    end function to_json
    
    !> Create dictionary from JSON string
    !>
    !> Parses a JSON string into a dictionary where all values are stored as strings.
    !> Nested objects are stored as JSON strings and can be parsed separately.
    !>
    !> Parameters:
    !>     json_str : character(len=*), intent(in)
    !>         JSON string to parse
    !>
    !> Returns:
    !>     dict_type: Dictionary containing parsed key-value pairs
    !>
    !> Example:
    !> ```fortran
    !> type(dict_type) :: parsed_dict
    !> character(len=*), parameter :: json = '{"name":"John","age":"30"}'
    !> parsed_dict = from_json(json)
    !> ```
    function from_json(json_str) result(dict)
        character(len=*), intent(in) :: json_str
        type(dict_type) :: dict
        dict = dict_from_json(json_str)
    end function from_json
    
    !> Set integer value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : integer(int32), intent(in)
    !>         32-bit integer value to store
    subroutine set_int32(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value
        call dict_set_int32(dict, key, value)
    end subroutine set_int32
    
    !> Set real64 value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : real(real64), intent(in)
    !>         64-bit real value to store
    subroutine set_real64(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value
        call dict_set_real64(dict, key, value)
    end subroutine set_real64
    
    !> Set string value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : character(len=*), intent(in)
    !>         String value to store
    subroutine set_string(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: value
        call dict_set_string(dict, key, value)
    end subroutine set_string
    
    !> Set dictionary value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : dict_type, intent(in)
    !>         Nested dictionary to store
    subroutine set_dict(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        type(dict_type), intent(in) :: value
        call dict_set_dict(dict, key, value)
    end subroutine set_dict
    
    !> Set real64 array value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : real(real64), intent(in)
    !>         64-bit real array to store as JSON
    subroutine set_real64_array(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value(:)
        call dict_set_real64_array(dict, key, value)
    end subroutine set_real64_array
    
    !> Set real32 array value in dictionary  
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : real(real32), intent(in)
    !>         32-bit real array to store as JSON
    subroutine set_real32_array(dict, key, value)
        use iso_fortran_env, only: real32
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real32), intent(in) :: value(:)
        call dict_set_real32_array(dict, key, value)
    end subroutine set_real32_array
    
    !> Set int32 array value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : integer(int32), intent(in)
    !>         32-bit integer array to store as JSON
    subroutine set_int32_array(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: value(:)
        call dict_set_int32_array(dict, key, value)
    end subroutine set_int32_array
    
    !> Set int64 array value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : integer(int64), intent(in)
    !>         64-bit integer array to store as JSON
    subroutine set_int64_array(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: value(:)
        call dict_set_int64_array(dict, key, value)
    end subroutine set_int64_array
    
    !> Get integer value
    function get_int32(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        integer(int32) :: value
        value = dict_get_int32(dict, key, found)
    end function get_int32
    
    !> Get real64 value
    function get_real64(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        real(real64) :: value
        value = dict_get_real64(dict, key, found)
    end function get_real64
    
    !> Get string value
    function get_string(dict, key, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        logical, intent(out), optional :: found
        character(len=1024) :: value
        value = dict_get_string(dict, key, found)
    end function get_string

    ! ========== MOLD-BASED GETTERS ==========
    
    !> Get integer value using mold pattern for type resolution
    !>
    !> Parameters:
    !>     dict : dict_type, intent(in)
    !>         Dictionary to search
    !>     key : character(len=*), intent(in)
    !>         Key to retrieve value for
    !>     mold : integer(int32), intent(in)
    !>         Mold parameter for type resolution (use int32_mold)
    !>     found : logical, intent(out), optional
    !>         Set to True if key found, False otherwise
    !>
    !> Returns:
    !>     integer(int32): Value associated with key, or 0 if not found
    !>
    !> Example:
    !> ```fortran
    !> use dict_molds
    !> integer(int32) :: count
    !> logical :: found
    !> count = dict_get(my_dict, 'count', int32_mold, found)
    !> ```
    function get_int32_mold(dict, key, mold, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        integer(int32), intent(in) :: mold
        logical, intent(out), optional :: found
        integer(int32) :: value
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        value = dict_get_int32(dict, key, found)
    end function get_int32_mold
    
    !> Get real64 value using mold pattern for type resolution
    !>
    !> Parameters:
    !>     dict : dict_type, intent(in)
    !>         Dictionary to search
    !>     key : character(len=*), intent(in)
    !>         Key to retrieve value for
    !>     mold : real(real64), intent(in)
    !>         Mold parameter for type resolution (use real64_mold)
    !>     found : logical, intent(out), optional
    !>         Set to True if key found, False otherwise
    !>
    !> Returns:
    !>     real(real64): Value associated with key, or 0.0 if not found
    !>
    !> Example:
    !> ```fortran
    !> use dict_molds
    !> real(real64) :: pi
    !> logical :: found
    !> pi = dict_get(my_dict, 'pi', real64_mold, found)
    !> ```
    function get_real64_mold(dict, key, mold, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: mold
        logical, intent(out), optional :: found
        real(real64) :: value
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        value = dict_get_real64(dict, key, found)
    end function get_real64_mold
    
    !> Get string value using mold pattern for type resolution
    !>
    !> Parameters:
    !>     dict : dict_type, intent(in)
    !>         Dictionary to search
    !>     key : character(len=*), intent(in)
    !>         Key to retrieve value for
    !>     mold : character(len=*), intent(in)
    !>         Mold parameter for type resolution (use string_mold)
    !>     found : logical, intent(out), optional
    !>         Set to True if key found, False otherwise
    !>
    !> Returns:
    !>     character(len=1024): Value associated with key, or empty string if not found
    !>
    !> Example:
    !> ```fortran
    !> use dict_molds
    !> character(len=256) :: name
    !> logical :: found
    !> name = dict_get(my_dict, 'name', string_mold, found)
    !> ```
    function get_string_mold(dict, key, mold, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        character(len=*), intent(in) :: mold
        logical, intent(out), optional :: found
        character(len=1024) :: value
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        value = dict_get_string(dict, key, found)
    end function get_string_mold
    
    !> Get dictionary value using mold pattern for type resolution
    !>
    !> Parameters:
    !>     dict : dict_type, intent(in)
    !>         Dictionary to search
    !>     key : character(len=*), intent(in)
    !>         Key to retrieve value for
    !>     mold : integer, intent(in)
    !>         Mold parameter for type resolution (use dict_mold)
    !>     found : logical, intent(out), optional
    !>         Set to True if key found, False otherwise
    !>
    !> Returns:
    !>     dict_type: Nested dictionary associated with key, or empty dict if not found
    !>
    !> Example:
    !> ```fortran
    !> use molds
    !> type(dict_type) :: nested
    !> logical :: found
    !> nested = dict_get(my_dict, 'config', dict_mold, found)
    !> ```
    function get_dict_mold(dict, key, mold, found) result(value)
        type(dict_type), intent(in) :: dict
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: mold
        logical, intent(out), optional :: found
        type(dict_type) :: value
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        value = dict_get_dict(dict, key, found)
    end function get_dict_mold

end module dict