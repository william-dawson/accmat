program dot_product
    use fbuf
    use molds
    use iso_fortran_env, only: real64
    use iso_c_binding, only: c_ptr, c_f_pointer
    implicit none
    
    integer, parameter :: n = 1000
    type(fbuf_type) :: buf_a, buf_b
    real(real64), pointer :: a_data(:), b_data(:)
    real(real64) :: result
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
    
    ! Compute dot product - function doesn't know data location
    result = dot_product_fbuf(buf_a, buf_b)
    
    write(*,*) 'HOST Dot product result:', result
    write(*,*) 'Expected result:', 667667000.0_real64
    
    ! Now test DEVICE version
    result = dot_product_fbuf_device(buf_a, buf_b)
    
    write(*,*) 'DEVICE Dot product result:', result
    
    ! Clean up
    call destroy(buf_a)
    call destroy(buf_b)

contains

    function dot_product_fbuf(a, b) result(dot)
        type(fbuf_type), intent(in) :: a, b
        real(real64) :: dot
        type(fbuf_type) :: host_a, host_b
        real(real64), pointer :: data_a(:), data_b(:)
        integer :: i, n_elements
        
        ! Sync to HOST for CPU computation
        host_a = sync(a, FBUF_HOST)
        host_b = sync(b, FBUF_HOST)
        
        ! Get array size
        n_elements = host_a%get_size()
        
        ! Now get clean typed pointers
        data_a => get_ptr(host_a, real64_mold)
        data_b => get_ptr(host_b, real64_mold)
        
        ! Compute dot product
        dot = 0.0_real64
        do i = 1, n_elements
            dot = dot + data_a(i) * data_b(i)
        end do
        
        ! Clean up sync'd buffers
        call destroy(host_a)
        call destroy(host_b)
    end function dot_product_fbuf

    function dot_product_fbuf_device(a, b) result(dot)
        type(fbuf_type), intent(in) :: a, b
        real(real64) :: dot
        type(fbuf_type) :: device_a, device_b
        real(real64), pointer :: data_a(:), data_b(:)
        integer :: i, n_elements
        
        ! Sync to DEVICE for GPU computation
        device_a = sync(a, FBUF_DEVICE)
        device_b = sync(b, FBUF_DEVICE)
        
        ! Get array size
        n_elements = device_a%get_size()
        
        ! Now get clean typed pointers
        data_a => get_ptr(device_a, real64_mold)
        data_b => get_ptr(device_b, real64_mold)
        
        ! Compute dot product using OpenACC
        dot = 0.0_real64
        !$acc parallel loop present(data_a, data_b) reduction(+:dot)
        do i = 1, n_elements
            dot = dot + data_a(i) * data_b(i)
        end do
        !$acc end parallel loop
        
        ! Clean up sync'd buffers
        call destroy(device_a)
        call destroy(device_b)
    end function dot_product_fbuf_device

end program dot_product
