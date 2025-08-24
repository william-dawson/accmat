module fbuf_mod
    use iso_fortran_env, only: real32, real64, int32, int64
    use iso_c_binding, only: c_ptr, c_null_ptr, c_associated, c_loc, c_f_pointer, c_size_t
    implicit none
    private

    ! Interface to C memcpy for efficient copying
    interface
        subroutine c_memcpy(dest, src, n) bind(c, name="memcpy")
            import :: c_ptr, c_size_t
            type(c_ptr), value :: dest
            type(c_ptr), value :: src
            integer(c_size_t), value :: n
        end subroutine
    end interface

    public :: fbuf, FBUF_HOST, FBUF_DEVICE
    public :: fbuf_create_real32_1d, fbuf_create_real64_1d, fbuf_create_int32_1d, fbuf_create_int64_1d
    public :: fbuf_sync_impl, fbuf_hardcopy_impl, fbuf_destroy_impl
    public :: fbuf_get_ptr_real32_1d, fbuf_get_ptr_real64_1d, fbuf_get_ptr_int32_1d, fbuf_get_ptr_int64_1d

    integer, parameter :: FBUF_HOST = 1
    integer, parameter :: FBUF_DEVICE = 2

    ! Data type constants
    integer, parameter :: FBUF_REAL32 = 1
    integer, parameter :: FBUF_REAL64 = 2
    integer, parameter :: FBUF_INT32 = 3
    integer, parameter :: FBUF_INT64 = 4

    type :: fbuf_data_t
        type(c_ptr) :: host_ptr = c_null_ptr
        type(c_ptr) :: device_ptr = c_null_ptr
        integer :: data_type = 0
        integer, allocatable :: shape(:)
        integer :: element_size = 0
        integer :: total_elements = 0
        integer :: ref_count = 0
        logical :: host_valid = .false.
        logical :: device_valid = .false.
    end type

    type :: fbuf
        type(fbuf_data_t), pointer :: data => null()
        integer :: preferred_location = FBUF_HOST
        contains
            procedure :: get_shape
            procedure :: get_size
            procedure :: sync => fbuf_sync_impl
            procedure :: destroy => fbuf_destroy_impl
            procedure :: hardcopy => fbuf_hardcopy_impl
            ! Note: get_ptr is handled through module-level generic interface
            final :: fbuf_finalize
    end type


    contains

    function fbuf_create_real32_1d(n, location) result(buf)
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf) :: buf
        real(real32), pointer :: data_array(:)
        integer :: target_location

        target_location = FBUF_HOST
        if (present(location)) target_location = location
        buf%preferred_location = target_location

        allocate(buf%data)
        buf%data%data_type = FBUF_REAL32
        buf%data%element_size = 4  ! real32 is 4 bytes
        buf%data%total_elements = n
        allocate(buf%data%shape(1))
        buf%data%shape(1) = n
        buf%data%ref_count = 1
        
        ! Allocate memory on requested location
        allocate(data_array(n))
        
        if (target_location == FBUF_HOST) then
            buf%data%host_ptr = c_loc(data_array(1))
            buf%data%host_valid = .true.
            buf%data%device_valid = .false.
        else if (target_location == FBUF_DEVICE) then
            !$acc enter data create(data_array(:))
            buf%data%device_ptr = c_loc(data_array(1))
            buf%data%device_valid = .true.
            buf%data%host_valid = .false.
        end if
    end function fbuf_create_real32_1d

    function fbuf_create_real64_1d(n, location) result(buf)
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf) :: buf
        real(real64), pointer :: data_array(:)
        integer :: target_location

        target_location = FBUF_HOST
        if (present(location)) target_location = location
        buf%preferred_location = target_location

        allocate(buf%data)
        buf%data%data_type = FBUF_REAL64
        buf%data%element_size = 8  ! real64 is 8 bytes
        buf%data%total_elements = n
        allocate(buf%data%shape(1))
        buf%data%shape(1) = n
        buf%data%ref_count = 1
        
        ! Allocate memory on requested location
        allocate(data_array(n))
        
        if (target_location == FBUF_HOST) then
            buf%data%host_ptr = c_loc(data_array(1))
            buf%data%host_valid = .true.
            buf%data%device_valid = .false.
        else if (target_location == FBUF_DEVICE) then
            !$acc enter data create(data_array(:))
            buf%data%device_ptr = c_loc(data_array(1))
            buf%data%device_valid = .true.
            buf%data%host_valid = .false.
        end if
    end function fbuf_create_real64_1d

    function fbuf_create_int32_1d(n, location) result(buf)
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf) :: buf
        integer(int32), pointer :: data_array(:)
        integer :: target_location

        target_location = FBUF_HOST
        if (present(location)) target_location = location
        buf%preferred_location = target_location

        allocate(buf%data)
        buf%data%data_type = FBUF_INT32
        buf%data%element_size = 4  ! int32 is 4 bytes
        buf%data%total_elements = n
        allocate(buf%data%shape(1))
        buf%data%shape(1) = n
        buf%data%ref_count = 1
        
        ! Allocate memory on requested location
        allocate(data_array(n))
        
        if (target_location == FBUF_HOST) then
            buf%data%host_ptr = c_loc(data_array(1))
            buf%data%host_valid = .true.
            buf%data%device_valid = .false.
        else if (target_location == FBUF_DEVICE) then
            !$acc enter data create(data_array(:))
            buf%data%device_ptr = c_loc(data_array(1))
            buf%data%device_valid = .true.
            buf%data%host_valid = .false.
        end if
    end function fbuf_create_int32_1d

    function fbuf_create_int64_1d(n, location) result(buf)
        integer, intent(in) :: n
        integer, intent(in), optional :: location
        type(fbuf) :: buf
        integer(int64), pointer :: data_array(:)
        integer :: target_location

        target_location = FBUF_HOST
        if (present(location)) target_location = location
        buf%preferred_location = target_location

        allocate(buf%data)
        buf%data%data_type = FBUF_INT64
        buf%data%element_size = 8  ! int64 is 8 bytes
        buf%data%total_elements = n
        allocate(buf%data%shape(1))
        buf%data%shape(1) = n
        buf%data%ref_count = 1
        
        ! Allocate memory on requested location
        allocate(data_array(n))
        
        if (target_location == FBUF_HOST) then
            buf%data%host_ptr = c_loc(data_array(1))
            buf%data%host_valid = .true.
            buf%data%device_valid = .false.
        else if (target_location == FBUF_DEVICE) then
            !$acc enter data create(data_array(:))
            buf%data%device_ptr = c_loc(data_array(1))
            buf%data%device_valid = .true.
            buf%data%host_valid = .false.
        end if
    end function fbuf_create_int64_1d

    function get_shape(this) result(shape_array)
        class(fbuf), intent(in) :: this
        integer, allocatable :: shape_array(:)
        
        if (associated(this%data) .and. allocated(this%data%shape)) then
            allocate(shape_array(size(this%data%shape)))
            shape_array = this%data%shape
        else
            allocate(shape_array(0))
        end if
    end function

    function get_size(this) result(total_size)
        class(fbuf), intent(in) :: this
        integer :: total_size
        
        if (associated(this%data)) then
            total_size = this%data%total_elements
        else
            total_size = 0
        end if
    end function

    function fbuf_sync_impl(this, location) result(new_buf)
        class(fbuf), intent(in) :: this
        integer, intent(in) :: location
        type(fbuf) :: new_buf
        real(real64), pointer :: data(:)
        integer :: total_bytes
        
        if (.not. associated(this%data)) then
            return
        end if
        
        ! Create new fbuf sharing the same data structure but increment ref count
        new_buf%data => this%data
        new_buf%data%ref_count = new_buf%data%ref_count + 1
        new_buf%preferred_location = location
        
        ! Ensure data is available at requested location
        select case(location)
        case(FBUF_HOST)
            if (.not. this%data%host_valid) then
                if (this%data%device_valid) then
                    ! Copy from DEVICE to HOST
                    call c_f_pointer(this%data%device_ptr, data, [this%data%total_elements])
                    !$acc update host(data(:))
                    this%data%host_ptr = this%data%device_ptr
                    this%data%host_valid = .true.
                end if
            end if
            
        case(FBUF_DEVICE)
            if (.not. this%data%device_valid) then
                if (this%data%host_valid) then
                    ! Copy from HOST to DEVICE
                    call c_f_pointer(this%data%host_ptr, data, [this%data%total_elements])
                    !$acc update device(data(:))
                    this%data%device_ptr = this%data%host_ptr
                    this%data%device_valid = .true.
                else if (.not. c_associated(this%data%device_ptr)) then
                    ! Allocate DEVICE memory
                    total_bytes = this%data%total_elements * this%data%element_size
                    ! DEVICE allocation would happen here
                    this%data%device_valid = .true.
                end if
            end if
        end select
    end function fbuf_sync_impl

    function fbuf_hardcopy_impl(this, location) result(new_buf)
        class(fbuf), intent(in) :: this
        integer, intent(in), optional :: location
        type(fbuf) :: new_buf
        integer :: target_location, n
        type(c_ptr) :: src_ptr, dst_ptr
        
        if (.not. associated(this%data)) return
        
        target_location = this%preferred_location
        if (present(location)) target_location = location
        n = this%data%total_elements
        
        ! Create new fbuf with same type and size
        select case(this%data%data_type)
        case(FBUF_REAL32)
            new_buf = fbuf_create_real32_1d(n, target_location)
        case(FBUF_REAL64)
            new_buf = fbuf_create_real64_1d(n, target_location)
        case(FBUF_INT32)
            new_buf = fbuf_create_int32_1d(n, target_location)
        case(FBUF_INT64)
            new_buf = fbuf_create_int64_1d(n, target_location)
        case default
            return  ! Unknown type
        end select
        
        ! Get source pointer
        if (this%data%host_valid) then
            src_ptr = this%data%host_ptr
        else if (this%data%device_valid) then
            src_ptr = this%data%device_ptr
        else
            return  ! No valid data to copy
        end if
        
        ! Get destination pointer and copy memory
        dst_ptr = new_buf%data%host_ptr
        if (c_associated(src_ptr) .and. c_associated(dst_ptr)) then
            ! Copy raw memory - works for any type
            call c_memcpy(dst_ptr, src_ptr, int(this%data%element_size * n, c_size_t))
        end if
        
        ! If target is DEVICE, sync to device
        if (target_location == FBUF_DEVICE) then
            ! Note: This requires the actual data pointer for OpenACC
            new_buf%data%device_ptr = new_buf%data%host_ptr
            new_buf%data%device_valid = .true.
        end if
    end function fbuf_hardcopy_impl

    function fbuf_get_ptr_real32_1d(this) result(data_ptr)
        class(fbuf), intent(inout) :: this
        real(real32), pointer :: data_ptr(:)
        type(c_ptr) :: c_ptr_result
        
        if (.not. associated(this%data) .or. this%data%data_type /= FBUF_REAL32) then
            nullify(data_ptr)
            return
        end if
        
        ! Return pointer to wherever the data currently is valid
        if (this%data%host_valid) then
            c_ptr_result = this%data%host_ptr
        else if (this%data%device_valid) then
            c_ptr_result = this%data%device_ptr
        else
            nullify(data_ptr)
            return
        end if
        
        ! Convert to Fortran pointer with correct size
        if (c_associated(c_ptr_result)) then
            call c_f_pointer(c_ptr_result, data_ptr, [this%data%total_elements])
        else
            nullify(data_ptr)
        end if
    end function fbuf_get_ptr_real32_1d

    function fbuf_get_ptr_real64_1d(this) result(data_ptr)
        class(fbuf), intent(inout) :: this
        real(real64), pointer :: data_ptr(:)
        type(c_ptr) :: c_ptr_result
        
        if (.not. associated(this%data) .or. this%data%data_type /= FBUF_REAL64) then
            nullify(data_ptr)
            return
        end if
        
        ! Return pointer to wherever the data currently is valid
        if (this%data%host_valid) then
            c_ptr_result = this%data%host_ptr
        else if (this%data%device_valid) then
            c_ptr_result = this%data%device_ptr
        else
            nullify(data_ptr)
            return
        end if
        
        ! Convert to Fortran pointer with correct size
        if (c_associated(c_ptr_result)) then
            call c_f_pointer(c_ptr_result, data_ptr, [this%data%total_elements])
        else
            nullify(data_ptr)
        end if
    end function fbuf_get_ptr_real64_1d

    function fbuf_get_ptr_int32_1d(this) result(data_ptr)
        class(fbuf), intent(inout) :: this
        integer(int32), pointer :: data_ptr(:)
        type(c_ptr) :: c_ptr_result
        
        if (.not. associated(this%data) .or. this%data%data_type /= FBUF_INT32) then
            nullify(data_ptr)
            return
        end if
        
        ! Return pointer to wherever the data currently is valid
        if (this%data%host_valid) then
            c_ptr_result = this%data%host_ptr
        else if (this%data%device_valid) then
            c_ptr_result = this%data%device_ptr
        else
            nullify(data_ptr)
            return
        end if
        
        ! Convert to Fortran pointer with correct size
        if (c_associated(c_ptr_result)) then
            call c_f_pointer(c_ptr_result, data_ptr, [this%data%total_elements])
        else
            nullify(data_ptr)
        end if
    end function fbuf_get_ptr_int32_1d

    function fbuf_get_ptr_int64_1d(this) result(data_ptr)
        class(fbuf), intent(inout) :: this
        integer(int64), pointer :: data_ptr(:)
        type(c_ptr) :: c_ptr_result
        
        if (.not. associated(this%data) .or. this%data%data_type /= FBUF_INT64) then
            nullify(data_ptr)
            return
        end if
        
        ! Return pointer to wherever the data currently is valid
        if (this%data%host_valid) then
            c_ptr_result = this%data%host_ptr
        else if (this%data%device_valid) then
            c_ptr_result = this%data%device_ptr
        else
            nullify(data_ptr)
            return
        end if
        
        ! Convert to Fortran pointer with correct size
        if (c_associated(c_ptr_result)) then
            call c_f_pointer(c_ptr_result, data_ptr, [this%data%total_elements])
        else
            nullify(data_ptr)
        end if
    end function fbuf_get_ptr_int64_1d

    subroutine fbuf_destroy_impl(this)
        class(fbuf), intent(inout) :: this
        
        if (.not. associated(this%data)) return
        
        this%data%ref_count = this%data%ref_count - 1
        
        if (this%data%ref_count <= 0) then
            ! Clean up DEVICE data if it exists (type-specific)
            if (this%data%device_valid .and. c_associated(this%data%device_ptr)) then
                call fbuf_cleanup_device_data(this)
            end if
            
            ! Clean up HOST memory since we own it (type-specific)
            if (this%data%host_valid .and. c_associated(this%data%host_ptr)) then
                call fbuf_cleanup_host_data(this)
            end if
            
            if (allocated(this%data%shape)) then
                deallocate(this%data%shape)
            end if
            
            deallocate(this%data)
        end if
        
        nullify(this%data)
    end subroutine fbuf_destroy_impl

    subroutine fbuf_cleanup_device_data(this)
        class(fbuf), intent(inout) :: this
        real(real32), pointer :: device_data_r32(:)
        real(real64), pointer :: device_data_r64(:)
        integer(int32), pointer :: device_data_i32(:)
        integer(int64), pointer :: device_data_i64(:)
        
        select case(this%data%data_type)
        case(FBUF_REAL32)
            call c_f_pointer(this%data%device_ptr, device_data_r32, [this%data%total_elements])
            !$acc exit data delete(device_data_r32(:))
        case(FBUF_REAL64)
            call c_f_pointer(this%data%device_ptr, device_data_r64, [this%data%total_elements])
            !$acc exit data delete(device_data_r64(:))
        case(FBUF_INT32)
            call c_f_pointer(this%data%device_ptr, device_data_i32, [this%data%total_elements])
            !$acc exit data delete(device_data_i32(:))
        case(FBUF_INT64)
            call c_f_pointer(this%data%device_ptr, device_data_i64, [this%data%total_elements])
            !$acc exit data delete(device_data_i64(:))
        end select
    end subroutine fbuf_cleanup_device_data

    subroutine fbuf_cleanup_host_data(this)
        class(fbuf), intent(inout) :: this
        real(real32), pointer :: host_data_r32(:)
        real(real64), pointer :: host_data_r64(:)
        integer(int32), pointer :: host_data_i32(:)
        integer(int64), pointer :: host_data_i64(:)
        
        select case(this%data%data_type)
        case(FBUF_REAL32)
            call c_f_pointer(this%data%host_ptr, host_data_r32, [this%data%total_elements])
            deallocate(host_data_r32)
        case(FBUF_REAL64)
            call c_f_pointer(this%data%host_ptr, host_data_r64, [this%data%total_elements])
            deallocate(host_data_r64)
        case(FBUF_INT32)
            call c_f_pointer(this%data%host_ptr, host_data_i32, [this%data%total_elements])
            deallocate(host_data_i32)
        case(FBUF_INT64)
            call c_f_pointer(this%data%host_ptr, host_data_i64, [this%data%total_elements])
            deallocate(host_data_i64)
        end select
    end subroutine fbuf_cleanup_host_data

    subroutine fbuf_finalize(this)
        type(fbuf), intent(inout) :: this
        call fbuf_destroy_impl(this)
    end subroutine

end module fbuf_mod
