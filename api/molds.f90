!> Common mold constants for type resolution across all modules
!>
!> This module provides mold constants used throughout the fbuf ecosystem
!> for generic interface type resolution. These molds eliminate ambiguity
!> in generic interfaces and enable clean, type-safe APIs.
!>
!> The mold pattern works by using a dummy parameter of the desired type
!> to help Fortran's compiler resolve which specific procedure to call
!> from a generic interface.
!>
!> Example Usage:
!> -------------
!> ```fortran
!> use molds
!> use dict
!> use fbuf
!> 
!> ! Dictionary usage
!> type(dict_type) :: my_dict
!> integer(int32) :: count
!> count = dict_get(my_dict, 'count', int32_mold)
!> 
!> ! fbuf usage  
!> type(fbuf_type) :: buf
!> buf = create(real64_mold, 1000, FBUF_HOST)
!> ```
module molds
    use iso_fortran_env, only: real32, real64, int32, int64
    implicit none
    
    public :: int32_mold, int64_mold, real32_mold, real64_mold, string_mold, dict_mold
    
    !> Mold constant for 32-bit integers
    integer(int32), parameter :: int32_mold = 0_int32
    
    !> Mold constant for 64-bit integers  
    integer(int64), parameter :: int64_mold = 0_int64
    
    !> Mold constant for 32-bit reals
    real(real32), parameter :: real32_mold = 0.0_real32
    
    !> Mold constant for 64-bit reals
    real(real64), parameter :: real64_mold = 0.0_real64
    
    !> Mold constant for strings
    character(len=1), parameter :: string_mold = 'x'
    
    !> Mold constant for dictionaries - use int64 to avoid ambiguity
    integer(int64), parameter :: dict_mold = 0_int64

end module molds