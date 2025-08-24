program vector_add
    use fbuf
    use molds
    use iso_fortran_env, only: real64
    use iso_c_binding, only: c_ptr, c_f_pointer
    implicit none
    
    integer, parameter :: n = 1000
    type(fbuf_type) :: buf_a, buf_b, buf_result, host_result
    real(real64), pointer :: a_data(:), b_data(:), result_data(:)
    integer :: i
    
    ! Create fbufs
    buf_a = create(real64_mold, n, FBUF_HOST)
    buf_b = create(real64_mold, n, FBUF_HOST)
    
    ! Get typed pointers to fill the data - fbuf knows where data is!
    a_data => get_ptr(buf_a, real64_mold)
    b_data => get_ptr(buf_b, real64_mold)
    
    ! Fill arrays with test data
    do i = 1, n
        a_data(i) = real(i, real64)
        b_data(i) = 2.0_real64 * real(i, real64)
    end do
    
    ! Add vectors - function doesn't know data location
    buf_result = vector_add_fbuf(buf_a, buf_b)
    
    ! Get pointer to result data and verify a few values
    result_data => get_ptr(buf_result, real64_mold)
    
    write(*,*) 'HOST Vector addition results (first 5 elements):'
    do i = 1, 5
        write(*,*) 'result[', i, '] =', result_data(i), ' (expected:', 3.0_real64 * real(i, real64), ')'
    end do
    
    ! Now test DEVICE version
    buf_result = vector_add_fbuf_device(buf_a, buf_b)
    
    ! Sync result to HOST for printing
    host_result = sync(buf_result, FBUF_HOST)
    result_data => get_ptr(host_result, real64_mold)
    
    write(*,*) 'DEVICE Vector addition results (first 5 elements):'
    do i = 1, 5
        write(*,*) 'result[', i, '] =', result_data(i), ' (expected:', 3.0_real64 * real(i, real64), ')'
    end do
    
    ! Clean up
    call destroy(host_result)
    
    ! Clean up
    call destroy(buf_a)
    call destroy(buf_b)
    call destroy(buf_result)

contains

    function vector_add_fbuf(a, b) result(result_buf)
        type(fbuf_type), intent(in) :: a, b
        type(fbuf_type) :: result_buf
        type(fbuf_type) :: host_a, host_b
        real(real64), pointer :: data_a(:), data_b(:), result_data(:)
        integer :: i, n_elements
        
        ! Sync to HOST for CPU computation
        host_a = sync(a, FBUF_HOST)
        host_b = sync(b, FBUF_HOST)
        
        ! Get array size
        n_elements = host_a%get_size()
        
        ! Create result buffer
        result_buf = create(real64_mold, n_elements, FBUF_HOST)
        
        ! Get typed pointers
        data_a => get_ptr(host_a, real64_mold)
        data_b => get_ptr(host_b, real64_mold)
        result_data => get_ptr(result_buf, real64_mold)
        
        ! Add vectors
        do i = 1, n_elements
            result_data(i) = data_a(i) + data_b(i)
        end do
        
        ! Clean up sync'd buffers
        call destroy(host_a)
        call destroy(host_b)
    end function vector_add_fbuf

    function vector_add_fbuf_device(a, b) result(result_buf)
        type(fbuf_type), intent(in) :: a, b
        type(fbuf_type) :: result_buf
        type(fbuf_type) :: device_a, device_b
        real(real64), pointer :: data_a(:), data_b(:), result_data(:)
        integer :: i, n_elements
        
        ! Sync to DEVICE for GPU computation
        device_a = sync(a, FBUF_DEVICE)
        device_b = sync(b, FBUF_DEVICE)
        
        ! Get array size
        n_elements = device_a%get_size()
        
        ! Create result buffer on DEVICE
        result_buf = create(real64_mold, n_elements, FBUF_DEVICE)
        
        ! Get typed pointers
        data_a => get_ptr(device_a, real64_mold)
        data_b => get_ptr(device_b, real64_mold)
        result_data => get_ptr(result_buf, real64_mold)
        
        ! Add vectors using OpenACC
        !$acc parallel loop present(data_a, data_b, result_data)
        do i = 1, n_elements
            result_data(i) = data_a(i) + data_b(i)
        end do
        !$acc end parallel loop
        
        ! Clean up sync'd buffers
        call destroy(device_a)
        call destroy(device_b)
    end function vector_add_fbuf_device

end program vector_add
