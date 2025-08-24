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
                        dict_set_int32, dict_set_real64, dict_set_string, dict_set_real64_array, &
                        dict_get_int32, dict_get_real64, dict_get_string
    use molds, only: int32_mold, real64_mold, string_mold
    use iso_fortran_env, only: real64, int32
    implicit none
    
    public :: dict_type
    public :: create, destroy, has_key, to_json
    public :: dict_set, dict_get
    
    ! Generic interfaces for setting values
    interface dict_set
        module procedure :: set_int32, set_real64, set_string, set_real64_array
    end interface
    
    ! Generic interfaces for getting values using mold pattern
    interface dict_get
        module procedure :: get_int32_mold, get_real64_mold, get_string_mold
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
    
    !> Set real64 array value in dictionary
    !>
    !> Parameters:
    !>     dict : dict_type, intent(inout)
    !>         Dictionary to modify
    !>     key : character(len=*), intent(in)
    !>         Key for the value
    !>     value : real(real64), intent(in)
    !>         Array of 64-bit real values to store
    subroutine set_real64_array(dict, key, value)
        type(dict_type), intent(inout) :: dict
        character(len=*), intent(in) :: key
        real(real64), intent(in) :: value(:)
        call dict_set_real64_array(dict, key, value)
    end subroutine set_real64_array
    
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

end module dict