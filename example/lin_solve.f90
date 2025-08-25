program lin_solve
    use fbuf
    use dict, only: dict_type, dict_set, dict_get, dict_create => create, dict_destroy => destroy
    use molds
    use iso_fortran_env, only: real64, int32
    implicit none
    
    integer, parameter :: n = 50  ! Problem size
    integer, parameter :: max_history = 1000  ! Maximum residual history
    type(fbuf_type) :: matrix_buf, x_buf, b_buf, r_buf, p_buf
    type(dict_type) :: options
    real(real64), pointer :: matrix(:), x(:), b(:), r(:), p(:)
    integer(int32) :: max_iter, iter, verbosity, i
    real(real64) :: tolerance, alpha, rsold, rsnew, norm_r
    real(real64) :: residual_history(max_history)
    logical :: found, converged
    character(len=1024) :: str_val
    
    write(*,*) 'Linear System Solver'
    write(*,*) ''
    
    ! Configuration
    max_iter = 1000
    tolerance = 1.0e-6_real64
    verbosity = 1
    
    ! Store parameters in dictionary
    options = dict_create()
    call dict_set(options, 'problem_size', n)
    call dict_set(options, 'max_iterations', max_iter)
    call dict_set(options, 'tolerance', tolerance)
    call dict_set(options, 'verbosity', verbosity)
    call dict_set(options, 'algorithm', 'steepest_descent')
    
    write(*,*) 'Configuration:'
    write(*,*) 'problem_size =', dict_get(options, 'problem_size', int32_mold, found)
    write(*,*) 'max_iterations =', dict_get(options, 'max_iterations', int32_mold, found)
    write(*,*) 'tolerance =', dict_get(options, 'tolerance', real64_mold, found)
    str_val = dict_get(options, 'algorithm', string_mold, found)
    write(*,*) 'algorithm = "' // trim(str_val) // '"'
    write(*,*) ''
    
    ! Allocate memory using fbuf on device (OpenACC)
    write(*,*) 'Allocating memory on device...'
    matrix_buf = create(real64_mold, n * n, FBUF_OACC)  ! Matrix as 1D array
    x_buf = create(real64_mold, n, FBUF_OACC)          ! Solution vector
    b_buf = create(real64_mold, n, FBUF_OACC)          ! Right-hand side
    r_buf = create(real64_mold, n, FBUF_OACC)          ! Residual vector
    p_buf = create(real64_mold, n, FBUF_OACC)          ! Work vector
    
    ! Get pointers to data
    matrix => get_ptr(matrix_buf, real64_mold)
    x => get_ptr(x_buf, real64_mold)
    b => get_ptr(b_buf, real64_mold)
    r => get_ptr(r_buf, real64_mold)
    p => get_ptr(p_buf, real64_mold)
    
    write(*,*) 'Allocated:', n*n, 'matrix elements and', n, 'vector elements each'
    write(*,*) ''
    
    ! Initialize problem
    call setup_problem(matrix_buf, x_buf, b_buf, n, options)
    
    write(*,*) 'Starting steepest descent iteration...'
    write(*,*) ''
    
    ! Steepest descent algorithm
    converged = .false.
    
    ! Initial residual: r = b - A*x
    call compute_residual(matrix_buf, x_buf, b_buf, r_buf, n)
    
    ! Get device pointer for initial residual computation
    r => get_ptr(r_buf, real64_mold)
    
    ! Compute initial residual norm using OpenACC
    rsold = 0.0_real64
    !$acc parallel loop reduction(+:rsold) present(r)
    do i = 1, n
        rsold = rsold + r(i) * r(i)
    end do
    !$acc end parallel loop
    norm_r = sqrt(rsold)
    
    ! Store initial residual
    residual_history(1) = norm_r
    
    if (verbosity >= 1) then
        write(*,*) 'iter =', 0, 'residual_norm =', norm_r
    end if
    
    do iter = 1, max_iter
        ! Check convergence
        if (norm_r < tolerance) then
            converged = .true.
            exit
        end if
        
        ! Compute A*r (store in p)
        call matrix_vector_mult(matrix_buf, r_buf, p_buf, n)
        
        ! Get fresh pointers for device computation
        r => get_ptr(r_buf, real64_mold)
        p => get_ptr(p_buf, real64_mold) 
        x => get_ptr(x_buf, real64_mold)
        
        ! Step size: alpha = (r'*r) / (r'*A*r) using OpenACC reduction
        alpha = 0.0_real64
        !$acc parallel loop reduction(+:alpha) present(r, p)
        do i = 1, n
            alpha = alpha + r(i) * p(i)
        end do
        !$acc end parallel loop
        alpha = rsold / alpha
        
        ! Update solution and residual using OpenACC
        !$acc parallel loop present(x, r, p)
        do i = 1, n
            x(i) = x(i) + alpha * r(i)      ! Update solution: x = x + alpha * r
            r(i) = r(i) - alpha * p(i)      ! Update residual: r = r - alpha * A*r
        end do
        !$acc end parallel loop
        
        ! New residual norm using OpenACC reduction
        rsnew = 0.0_real64
        !$acc parallel loop reduction(+:rsnew) present(r)
        do i = 1, n
            rsnew = rsnew + r(i) * r(i)
        end do
        !$acc end parallel loop
        norm_r = sqrt(rsnew)
        
        ! Store residual in history
        residual_history(iter + 1) = norm_r
        
        if (verbosity >= 1 .and. mod(iter, 10) == 0) then
            write(*,*) 'iter =', iter, 'residual_norm =', norm_r
        end if
        
        rsold = rsnew
    end do
    
    write(*,*) ''
    if (converged) then
        write(*,*) 'Converged in', iter, 'iterations'
    else
        write(*,*) 'Did not converge in', max_iter, 'iterations'
    end if
    write(*,*) 'Final residual norm =', norm_r
    write(*,*) ''
    
    ! Solution statistics
    ! Sync x_buf to HOST to access final solution for printing
    x_buf = sync(x_buf, FBUF_HOST)
    x => get_ptr(x_buf, real64_mold)
    
    write(*,*) 'Solution statistics:'
    write(*,*) 'min(x) =', minval(x)
    write(*,*) 'max(x) =', maxval(x)
    write(*,*) 'mean(x) =', sum(x) / size(x)
    write(*,*) ''
    
    ! Store results in dictionary
    if (converged) then
        call dict_set(options, 'converged', 'true')
    else
        call dict_set(options, 'converged', 'false')
    end if
    call dict_set(options, 'final_iterations', iter)
    call dict_set(options, 'final_residual', norm_r)
    
    ! Store residual history using new array utilities
    call dict_set(options, 'residual_history', residual_history(1:iter+1))
    
    write(*,*) 'Final results stored in dictionary:'
    str_val = dict_get(options, 'converged', string_mold, found)
    write(*,*) 'converged = "' // trim(str_val) // '"'
    write(*,*) 'final_iterations =', dict_get(options, 'final_iterations', int32_mold, found)
    write(*,*) 'final_residual =', dict_get(options, 'final_residual', real64_mold, found)
    str_val = dict_get(options, 'residual_history', string_mold, found)
    write(*,*) 'residual_history =', trim(str_val)
    write(*,*) ''
    
    ! Clean up
    call destroy(matrix_buf)
    call destroy(x_buf)
    call destroy(b_buf)
    call destroy(r_buf)
    call destroy(p_buf)
    call dict_destroy(options)
    
    write(*,*) 'Memory cleaned up - solver complete'

contains

    !> Setup the linear system A*x = b
    subroutine setup_problem(A_buf, x_buf, b_buf, n, opts)
        type(fbuf_type), intent(inout) :: A_buf, x_buf, b_buf
        integer, intent(in) :: n
        type(dict_type), intent(in) :: opts
        integer :: i, j, idx
        real(real64) :: off_diag
        logical :: verb_found
        integer :: verb_level
        real(real64), pointer :: A(:), x(:), b(:)
        
        verb_level = dict_get(opts, 'verbosity', int32_mold, verb_found)
        
        if (verb_level >= 2) then
            write(*,*) 'Setting up symmetric diagonally dominant matrix...'
        end if
        
        ! Get device pointers directly (data already on device)
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
        
        ! Get device pointers directly
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
        
        ! Get device pointers directly
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

end program lin_solve