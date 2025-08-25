program lin_solve
    use fbuf
    use dict, only: dict_type, dict_set, dict_get, dict_create => create, dict_destroy => destroy, print_json
    use molds
    use iso_fortran_env, only: real64, int32
    implicit none
    
    integer, parameter :: n = 50  ! Problem size
    integer, parameter :: max_history = 1000  ! Maximum residual history
    type(fbuf_type) :: matrix_buf, x_buf, b_buf, r_buf, p_buf
    type(dict_type) :: kwargs
    real(real64), pointer :: x(:)
    real(real64) :: residual_history(max_history)
    
    ! Store parameters in kwargs dictionary
    kwargs = dict_create()
    call dict_set(kwargs, 'problem_size', n)
    call dict_set(kwargs, 'max_iterations', 1000)
    call dict_set(kwargs, 'tolerance', 1e-6_real64)
    call dict_set(kwargs, 'verbosity', 1)
    call dict_set(kwargs, 'algorithm', 'steepest_descent')
    
    write(*,*) 'Configuration:'
    call print_json(kwargs)
    write(*,*) ''
    
    ! Allocate memory using fbuf on device (OpenACC)
    write(*,*) 'Allocating memory on device...'
    matrix_buf = create(real64_mold, n * n, FBUF_OACC)  ! Matrix as 1D array
    x_buf = create(real64_mold, n, FBUF_OACC)          ! Solution vector
    b_buf = create(real64_mold, n, FBUF_OACC)          ! Right-hand side
    r_buf = create(real64_mold, n, FBUF_OACC)          ! Residual vector
    p_buf = create(real64_mold, n, FBUF_OACC)          ! Work vector
    
    ! No need to get pointers here - subroutines will handle this
    
    write(*,*) 'Allocated:', n*n, 'matrix elements and', n, 'vector elements each'
    write(*,*) ''
    
    ! Initialize problem
    call setup_problem(matrix_buf, x_buf, b_buf, kwargs)
    
    write(*,*) 'Starting steepest descent iteration...'
    write(*,*) ''
    
    ! Run the steepest descent solver
    call steepest_descent_solver(matrix_buf, x_buf, b_buf, r_buf, p_buf, &
                                 kwargs, residual_history)
    
    ! Solution statistics
    ! Sync x_buf to HOST to access final solution for printing
    x_buf = sync(x_buf, FBUF_HOST)
    x => get_ptr(x_buf, real64_mold)
    
    write(*,*) 'Solution statistics:'
    write(*,*) 'min(x) =', minval(x)
    write(*,*) 'max(x) =', maxval(x)
    write(*,*) 'mean(x) =', sum(x) / size(x)
    write(*,*) ''
    
    write(*,*) 'Final results:'
    call print_json(kwargs)
    write(*,*) ''
    
    ! Clean up
    call destroy(matrix_buf)
    call destroy(x_buf)
    call destroy(b_buf)
    call destroy(r_buf)
    call destroy(p_buf)
    call dict_destroy(kwargs)
    
    write(*,*) 'Memory cleaned up - solver complete'

contains

    !> Steepest descent solver for linear system A*x = b
    subroutine steepest_descent_solver(matrix_buf, x_buf, b_buf, r_buf, p_buf, &
                                       kwargs, residual_history)
        type(fbuf_type), intent(inout) :: matrix_buf, x_buf, b_buf, r_buf, p_buf
        type(dict_type), intent(inout) :: kwargs
        real(real64), intent(inout) :: residual_history(:)
        
        integer(int32) :: max_iter, iter, verbosity, n
        real(real64) :: tolerance, alpha, rsold, rsnew, norm_r
        logical :: found, converged
        
        ! Get parameters from kwargs
        n = dict_get(kwargs, 'problem_size', int32_mold, found)
        max_iter = dict_get(kwargs, 'max_iterations', int32_mold, found)
        tolerance = dict_get(kwargs, 'tolerance', real64_mold, found)
        verbosity = dict_get(kwargs, 'verbosity', int32_mold, found)
        
        ! Steepest descent algorithm
        converged = .false.
        
        ! Initial residual: r = b - A*x
        call compute_residual(matrix_buf, x_buf, b_buf, r_buf, n)
        
        ! Compute initial residual norm
        norm_r = compute_norm(r_buf, n)
        rsold = norm_r * norm_r
        
        ! Store initial residual
        residual_history(1) = norm_r
        
        if (verbosity >= 1) then
            write(*,*) 'iter =', 0, 'residual_norm =', norm_r
        end if
        
        do iter = 1, max_iter
            ! Compute A*r (store in p)
            call matrix_vector_mult(matrix_buf, r_buf, p_buf, n)
            
            ! Step size: alpha = (r'*r) / (r'*A*r)
            alpha = rsold / compute_dot_product(r_buf, p_buf, n)
            
            ! Update solution: x = x + alpha * r
            call vector_axpy(x_buf, r_buf, alpha, n)
            
            ! Update residual: r = r - alpha * A*r (p)
            call vector_axpy(r_buf, p_buf, -alpha, n)
            
            ! New residual norm
            norm_r = compute_norm(r_buf, n)
            rsnew = norm_r * norm_r
            
            ! Store residual in history
            residual_history(iter + 1) = norm_r
            
            if (verbosity >= 1 .and. mod(iter, 10) == 0) then
                write(*,*) 'iter =', iter, 'residual_norm =', norm_r
            end if
            
            ! Check convergence
            if (norm_r < tolerance) then
                converged = .true.
                exit
            end if
            
            rsold = rsnew
        end do
        
        write(*,*) ''
        if (converged) then
            write(*,*) 'Converged in', iter, 'iterations'
            call dict_set(kwargs, 'converged', 'true')
            call dict_set(kwargs, 'final_iterations', iter)
        else
            write(*,*) 'Did not converge in', max_iter, 'iterations'
            call dict_set(kwargs, 'converged', 'false')
            call dict_set(kwargs, 'final_iterations', max_iter)
        end if
        write(*,*) 'Final residual norm =', norm_r
        write(*,*) ''
        
        ! Store results in kwargs
        call dict_set(kwargs, 'residual_history', residual_history(1:iter+1))
    end subroutine steepest_descent_solver

    !> Setup the linear system A*x = b
    subroutine setup_problem(A_buf, x_buf, b_buf, kwargs)
        type(fbuf_type), intent(inout) :: A_buf, x_buf, b_buf
        type(dict_type), intent(in) :: kwargs
        integer :: i, j, idx, n
        real(real64) :: off_diag
        logical :: verb_found
        integer :: verb_level
        real(real64), pointer :: A(:), x(:), b(:)
        
        n = dict_get(kwargs, 'problem_size', int32_mold, verb_found)
        verb_level = dict_get(kwargs, 'verbosity', int32_mold, verb_found)
        
        if (verb_level >= 2) then
            write(*,*) 'Setting up symmetric diagonally dominant matrix...'
        end if
        
        ! Sync all buffers to device
        A_buf = sync(A_buf, FBUF_OACC)
        x_buf = sync(x_buf, FBUF_OACC)
        b_buf = sync(b_buf, FBUF_OACC)
        
        ! Get device pointers
        A => get_ptr(A_buf, real64_mold)
        x => get_ptr(x_buf, real64_mold)
        b => get_ptr(b_buf, real64_mold)
        
        ! Initialize on device using OpenACC
        !$acc parallel loop present(A, x)
        do i = 1, n * n
            A(i) = 0.0_real64
        end do
        !$acc end parallel loop
        
        !$acc parallel loop present(x)
        do i = 1, n
            x(i) = 0.0_real64  ! Initial guess
        end do
        !$acc end parallel loop
        
        ! Create symmetric diagonally dominant matrix using OpenACC
        !$acc parallel loop collapse(2) present(A)
        do i = 1, n
            do j = 1, n
                idx = (i-1) * n + j  ! Row-major indexing
                if (i == j) then
                    ! Diagonal dominance
                    A(idx) = real(n, real64) + 2.0_real64
                else
                    ! Symmetric off-diagonal
                    off_diag = 1.0_real64 / real(i + j, real64)
                    A(idx) = off_diag
                end if
            end do
        end do
        !$acc end parallel loop
        
        ! Right-hand side (simple pattern) using OpenACC
        !$acc parallel loop present(b)
        do i = 1, n
            b(i) = sin(real(i, real64) * 3.14159_real64 / real(n, real64)) + 0.1_real64 * real(i, real64)
        end do
        !$acc end parallel loop
        
        if (verb_level >= 2) then
            write(*,*) 'Problem setup complete on device'
            ! Note: For diagnostic messages, would need to sync to host
            ! Keeping device-based computation for performance
        end if
    end subroutine setup_problem
    
    !> Compute residual r = b - A*x
    subroutine compute_residual(A_buf, x_buf, b_buf, r_buf, n)
        type(fbuf_type), intent(inout) :: A_buf, x_buf, b_buf, r_buf
        integer, intent(in) :: n
        real(real64), pointer :: A(:), x(:), b(:), r(:)
        integer :: i
        
        ! Sync all buffers to device
        A_buf = sync(A_buf, FBUF_OACC)
        x_buf = sync(x_buf, FBUF_OACC)
        b_buf = sync(b_buf, FBUF_OACC)
        r_buf = sync(r_buf, FBUF_OACC)
        
        ! Get device pointers
        A => get_ptr(A_buf, real64_mold)
        x => get_ptr(x_buf, real64_mold)
        b => get_ptr(b_buf, real64_mold)
        r => get_ptr(r_buf, real64_mold)
        
        ! First compute A*x (store in r temporarily)
        call matrix_vector_mult(A_buf, x_buf, r_buf, n)
        
        ! Then r = b - A*x using OpenACC
        !$acc parallel loop present(r, b)
        do i = 1, n
            r(i) = b(i) - r(i)
        end do
        !$acc end parallel loop
    end subroutine compute_residual
    
    !> Matrix-vector multiplication: result = A * vec
    subroutine matrix_vector_mult(A_buf, vec_buf, result_buf, n)
        type(fbuf_type), intent(inout) :: A_buf, vec_buf, result_buf
        integer, intent(in) :: n
        integer :: i, j, idx
        real(real64), pointer :: A(:), vec(:), result(:)
        
        ! Sync all buffers to device
        A_buf = sync(A_buf, FBUF_OACC)
        vec_buf = sync(vec_buf, FBUF_OACC)
        result_buf = sync(result_buf, FBUF_OACC)
        
        ! Get device pointers
        A => get_ptr(A_buf, real64_mold)
        vec => get_ptr(vec_buf, real64_mold)
        result => get_ptr(result_buf, real64_mold)
        
        ! Matrix-vector multiplication using OpenACC
        !$acc parallel loop present(result, A, vec)
        do i = 1, n
            result(i) = 0.0_real64
            !$acc loop seq
            do j = 1, n
                idx = (i-1) * n + j
                result(i) = result(i) + A(idx) * vec(j)
            end do
        end do
        !$acc end parallel loop
    end subroutine matrix_vector_mult

    !> Compute the 2-norm of a vector: ||v||_2
    function compute_norm(v_buf, n) result(norm)
        type(fbuf_type), intent(inout) :: v_buf
        integer, intent(in) :: n
        real(real64) :: norm
        type(fbuf_type) :: device_v
        real(real64), pointer :: v(:)
        real(real64) :: sum_sq
        integer :: i
        
        ! Sync to device for computation
        device_v = sync(v_buf, FBUF_OACC)
        v => get_ptr(device_v, real64_mold)
        
        sum_sq = 0.0_real64
        !$acc parallel loop reduction(+:sum_sq) present(v)
        do i = 1, n
            sum_sq = sum_sq + v(i) * v(i)
        end do
        !$acc end parallel loop
        
        norm = sqrt(sum_sq)
        
        call destroy(device_v)
    end function compute_norm

    !> Compute dot product of two vectors: u^T * v
    function compute_dot_product(u_buf, v_buf, n) result(dot_prod)
        type(fbuf_type), intent(inout) :: u_buf, v_buf
        integer, intent(in) :: n
        real(real64) :: dot_prod
        type(fbuf_type) :: device_u, device_v
        real(real64), pointer :: u(:), v(:)
        integer :: i
        
        ! Sync both vectors to device for computation
        device_u = sync(u_buf, FBUF_OACC)
        device_v = sync(v_buf, FBUF_OACC)
        
        u => get_ptr(device_u, real64_mold)
        v => get_ptr(device_v, real64_mold)
        
        dot_prod = 0.0_real64
        !$acc parallel loop reduction(+:dot_prod) present(u, v)
        do i = 1, n
            dot_prod = dot_prod + u(i) * v(i)
        end do
        !$acc end parallel loop
        
        call destroy(device_u)
        call destroy(device_v)
    end function compute_dot_product

    !> Vector AXPY operation: y = y + alpha * x
    subroutine vector_axpy(y_buf, x_buf, alpha, n)
        type(fbuf_type), intent(inout) :: y_buf, x_buf
        real(real64), intent(in) :: alpha
        integer, intent(in) :: n
        type(fbuf_type) :: device_x, device_y
        real(real64), pointer :: x(:), y(:)
        integer :: i
        
        ! Sync both vectors to device for computation
        device_x = sync(x_buf, FBUF_OACC)
        device_y = sync(y_buf, FBUF_OACC)
        
        x => get_ptr(device_x, real64_mold)
        y => get_ptr(device_y, real64_mold)
        
        !$acc parallel loop present(x, y)
        do i = 1, n
            y(i) = y(i) + alpha * x(i)
        end do
        !$acc end parallel loop
        
        ! Copy result back to original buffer
        y_buf = device_y
        
        call destroy(device_x)
        call destroy(device_y)
    end subroutine vector_axpy

end program lin_solve
