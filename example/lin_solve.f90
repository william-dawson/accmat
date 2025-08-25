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
    integer(int32) :: max_iter, iter, verbosity
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
    
    ! Allocate memory using fbuf
    write(*,*) 'Allocating memory...'
    matrix_buf = create(real64_mold, n * n, FBUF_HOST)  ! Matrix as 1D array
    x_buf = create(real64_mold, n, FBUF_HOST)          ! Solution vector
    b_buf = create(real64_mold, n, FBUF_HOST)          ! Right-hand side
    r_buf = create(real64_mold, n, FBUF_HOST)          ! Residual vector
    p_buf = create(real64_mold, n, FBUF_HOST)          ! Work vector
    
    ! Get pointers to data
    matrix => get_ptr(matrix_buf, real64_mold)
    x => get_ptr(x_buf, real64_mold)
    b => get_ptr(b_buf, real64_mold)
    r => get_ptr(r_buf, real64_mold)
    p => get_ptr(p_buf, real64_mold)
    
    write(*,*) 'Allocated:', n*n, 'matrix elements and', n, 'vector elements each'
    write(*,*) ''
    
    ! Initialize problem
    call setup_problem(matrix, x, b, n, options)
    
    write(*,*) 'Starting steepest descent iteration...'
    write(*,*) ''
    
    ! Steepest descent algorithm
    converged = .false.
    
    ! Initial residual: r = b - A*x
    call compute_residual(matrix, x, b, r, n)
    rsold = dot_product(r, r)
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
        call matrix_vector_mult(matrix, r, p, n)
        
        ! Step size: alpha = (r'*r) / (r'*A*r)
        alpha = rsold / dot_product(r, p)
        
        ! Update solution: x = x + alpha * r
        x = x + alpha * r
        
        ! Update residual: r = r - alpha * A*r
        r = r - alpha * p
        
        ! New residual norm
        rsnew = dot_product(r, r)
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
    subroutine setup_problem(A, x, b, n, opts)
        real(real64), intent(inout) :: A(:), x(:), b(:)
        integer, intent(in) :: n
        type(dict_type), intent(in) :: opts
        integer :: i, j, idx
        real(real64) :: off_diag
        logical :: verb_found
        integer :: verb_level
        
        verb_level = dict_get(opts, 'verbosity', int32_mold, verb_found)
        
        if (verb_level >= 2) then
            write(*,*) 'Setting up symmetric diagonally dominant matrix...'
        end if
        
        ! Initialize
        A = 0.0_real64
        x = 0.0_real64  ! Initial guess
        
        ! Create symmetric diagonally dominant matrix
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
        
        ! Right-hand side (simple pattern)
        do i = 1, n
            b(i) = sin(real(i, real64) * 3.14159_real64 / real(n, real64)) + 0.1_real64 * real(i, real64)
        end do
        
        if (verb_level >= 2) then
            write(*,*) 'Problem setup complete'
            write(*,*) 'A(1,1) =', A(1), 'A(n,n) =', A(n*n)
            write(*,*) 'b(1) =', b(1), 'b(n) =', b(n)
        end if
    end subroutine setup_problem
    
    !> Compute residual r = b - A*x
    subroutine compute_residual(A, x, b, r, n)
        real(real64), intent(in) :: A(:), x(:), b(:)
        real(real64), intent(out) :: r(:)
        integer, intent(in) :: n
        
        ! First compute A*x
        call matrix_vector_mult(A, x, r, n)
        
        ! Then r = b - A*x
        r = b - r
    end subroutine compute_residual
    
    !> Matrix-vector multiplication: result = A * vec
    subroutine matrix_vector_mult(A, vec, result, n)
        real(real64), intent(in) :: A(:), vec(:)
        real(real64), intent(out) :: result(:)
        integer, intent(in) :: n
        integer :: i, j, idx
        
        do i = 1, n
            result(i) = 0.0_real64
            do j = 1, n
                idx = (i-1) * n + j
                result(i) = result(i) + A(idx) * vec(j)
            end do
        end do
    end subroutine matrix_vector_mult

end program lin_solve