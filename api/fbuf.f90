!> Smart pointer for managing data on HOST and DEVICE
!>
!> The fbuf module provides smart pointers that can automatically manage
!> data placement between HOST and DEVICE memory. Each smart pointer tracks
!> shared data and handles synchronization transparently.
!>
!> Key Features:
!> ------------
!> - Automatic memory management between HOST and DEVICE
!> - Reference counting for shared data
!> - Generic interfaces using mold pattern for type safety
!> - OpenACC integration for GPU memory operations
!>
!> Example Usage:
!> -------------
!> ```fortran
!> use fbuf
!> use molds
!> 
!> type(fbuf_type) :: buf
!> real(real64), pointer :: data(:)
!> 
!> ! Create buffer on HOST
!> buf = create(real64_mold, 1000, FBUF_HOST)
!> data => get_ptr(buf, real64_mold)
!> 
!> ! Use data...
!> data(1:10) = [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]
!> 
!> ! Clean up
!> call destroy(buf)
!> ```
module fbuf
    use fbuf_mod, only: fbuf_type => fbuf, &
                        fbuf_create_real32_1d, fbuf_create_real64_1d, &
                        fbuf_create_int32_1d, fbuf_create_int64_1d, &
                        fbuf_sync_impl, fbuf_hardcopy_impl, fbuf_destroy_impl, &
                        fbuf_get_ptr_real32_1d, fbuf_get_ptr_real64_1d, &
                        fbuf_get_ptr_int32_1d, fbuf_get_ptr_int64_1d, &
                        FBUF_HOST, FBUF_OACC
    use molds, only: real32_mold, real64_mold, int32_mold, int64_mold
    use iso_fortran_env, only: real32, real64, int32, int64
    use iso_c_binding, only: c_ptr
    implicit none
    
    public :: fbuf_type, FBUF_HOST, FBUF_OACC
    public :: create, sync, hardcopy, destroy, get_ptr
    
    ! Generic interfaces for different types
    interface create
        module procedure :: create_real32, create_real64, create_int32, create_int64
    end interface
    
    interface get_ptr
        module procedure :: get_ptr_real32, get_ptr_real64, get_ptr_int32, get_ptr_int64
    end interface
    
contains

    !> Create a new smart pointer with allocated memory (real32)
    !>
    !> Parameters:
    !>     mold : real(real32), intent(in)
    !>         Mold parameter for type resolution (use real32_mold)
    !>     n : integer, intent(in)
    !>         Number of elements to allocate
    !>     location : integer, intent(in), optional
    !>         Preferred location (FBUF_HOST or FBUF_OACC)
    !>
    !> Returns:
    !>     fbuf_type: Smart pointer to allocated memory
    !>
    !> Example:
    !> ```fortran
    !> use molds
    !> buf = create(real32_mold, 1000, FBUF_HOST)
    !> ```
    function create_real32(mold, n, location) result(buf)
        real(real32), intent(in) :: mold
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf_type) :: buf
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        buf = fbuf_create_real32_1d(n, location)
    end function create_real32

    !> Create a new smart pointer with allocated memory (real64)
    function create_real64(mold, n, location) result(buf)
        real(real64), intent(in) :: mold
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf_type) :: buf
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        buf = fbuf_create_real64_1d(n, location)
    end function create_real64

    !> Create a new smart pointer with allocated memory (int32)
    function create_int32(mold, n, location) result(buf)
        integer(int32), intent(in) :: mold
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf_type) :: buf
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        buf = fbuf_create_int32_1d(n, location)
    end function create_int32

    !> Create a new smart pointer with allocated memory (int64)
    function create_int64(mold, n, location) result(buf)
        integer(int64), intent(in) :: mold
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf_type) :: buf
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        buf = fbuf_create_int64_1d(n, location)
    end function create_int64


    !> Synchronize data to a specific location and return new fbuf
    !>
    !> Creates a new fbuf that has data available at the requested location.
    !> If data is not currently valid at that location, it will be copied
    !> from the other location. The original fbuf is not modified.
    !>
    !> Parameters
    !> ----------
    !> this : fbuf
    !>     Smart pointer to synchronize from
    !> location : integer
    !>     Target location (FBUF_HOST or FBUF_OACC)
    !>
    !> Returns
    !> -------
    !> type(fbuf_type)
    !>     New smart pointer with data at the requested location
    !>
    !> Examples
    !> --------
    !> ```fortran
    !> type(fbuf_type) :: buf, host_buf, device_buf
    !> real(real64), pointer :: host_data(:)
    !> 
    !> buf = create(fbuf_real64, 1000, FBUF_HOST)
    !> 
    !> ! Get HOST fbuf (no copy needed, shares data)
    !> host_buf = sync(buf, FBUF_HOST)
    !> host_data => get_ptr(host_buf, fbuf_real64)
    !> 
    !> ! Get DEVICE fbuf (copies from HOST to DEVICE)
    !> device_buf = sync(buf, FBUF_OACC)
    !> 
    !> ! Later sync back to HOST (copies from DEVICE to HOST)
    !> host_buf = sync(device_buf, FBUF_HOST)
    !> ```
    function sync(this, location) result(new_buf)
        class(fbuf_type), intent(in) :: this
        integer, intent(in) :: location
        type(fbuf_type) :: new_buf
        
        new_buf = fbuf_sync_impl(this, location)
    end function sync

    !> Create an independent copy of the smart pointer data
    !>
    !> Creates a new smart pointer with an independent copy of the data.
    !> Unlike shared pointers, modifications to the copy won't affect the original.
    !> The copy can be created at a different location (HOST or DEVICE).
    !>
    !> Parameters
    !> ----------
    !> this : fbuf
    !>     Smart pointer to copy from
    !> location : integer, optional
    !>     Preferred location for the copy (FBUF_HOST or FBUF_OACC)
    !>     Default uses the same location as the source
    !>
    !> Returns
    !> -------
    !> fbuf
    !>     New smart pointer with independent copy of the data
    !>
    !> Examples
    !> --------
    !> ```fortran
    !> type(fbuf) :: original, copy_host, copy_device
    !> 
    !> original = create(1000, FBUF_HOST)
    !> 
    !> ! Create independent copy on HOST
    !> copy_host = hardcopy(original)
    !> 
    !> ! Create independent copy on DEVICE
    !> copy_device = hardcopy(original, FBUF_OACC)
    !> ```
    function hardcopy(this, location) result(new_buf)
        class(fbuf_type), intent(in) :: this
        integer, intent(in), optional :: location
        type(fbuf_type) :: new_buf
        
        new_buf = fbuf_hardcopy_impl(this, location)
    end function hardcopy

    !> Clean up smart pointer resources
    !>
    !> Explicitly destroys a smart pointer and decrements the reference count
    !> of the shared data. When the last smart pointer to data is destroyed,
    !> the shared data is automatically freed. This is also called automatically
    !> when smart pointers go out of scope.
    !>
    !> Parameters
    !> ----------
    !> this : fbuf
    !>     Smart pointer to destroy
    !>
    !> Examples
    !> --------
    !> ```fortran
    !> type(fbuf) :: buf1, buf2
    !> 
    !> buf1 = create(1000)
    !> 
    !> ! Explicitly clean up (optional - automatic on scope exit)
    !> call destroy(buf1)
    !> ```
    subroutine destroy(this)
        class(fbuf_type), intent(inout) :: this
        call fbuf_destroy_impl(this)
    end subroutine destroy

    !> Get a typed Fortran pointer to the data (real32)
    !>
    !> Parameters:
    !>     this : fbuf_type, intent(inout)
    !>         Smart pointer to get data from
    !>     mold : real(real32), intent(in)
    !>         Mold parameter for type resolution (use real32_mold)
    !>
    !> Returns:
    !>     real(real32), pointer: Fortran pointer to the data array
    !>
    !> Example:
    !> ```fortran
    !> use molds
    !> real(real32), pointer :: data(:)
    !> data => get_ptr(buf, real32_mold)
    !> ```
    function get_ptr_real32(this, mold) result(data_ptr)
        class(fbuf_type), intent(inout) :: this
        real(real32), intent(in) :: mold
        real(real32), pointer :: data_ptr(:)
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        data_ptr => fbuf_get_ptr_real32_1d(this)
    end function get_ptr_real32

    !> Get a typed Fortran pointer to the data (real64)
    function get_ptr_real64(this, mold) result(data_ptr)
        class(fbuf_type), intent(inout) :: this
        real(real64), intent(in) :: mold
        real(real64), pointer :: data_ptr(:)
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        data_ptr => fbuf_get_ptr_real64_1d(this)
    end function get_ptr_real64

    !> Get a typed Fortran pointer to the data (int32)
    function get_ptr_int32(this, mold) result(data_ptr)
        class(fbuf_type), intent(inout) :: this
        integer(int32), intent(in) :: mold
        integer(int32), pointer :: data_ptr(:)
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        data_ptr => fbuf_get_ptr_int32_1d(this)
    end function get_ptr_int32

    !> Get a typed Fortran pointer to the data (int64)
    function get_ptr_int64(this, mold) result(data_ptr)
        class(fbuf_type), intent(inout) :: this
        integer(int64), intent(in) :: mold
        integer(int64), pointer :: data_ptr(:)
        
        ! Use mold to suppress unused parameter warning
        if (.false.) print *, mold
        
        data_ptr => fbuf_get_ptr_int64_1d(this)
    end function get_ptr_int64

end module fbuf