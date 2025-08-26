# AI Built Fortran Style Helpers

This project should define an opinionated programming style for Fortran that will help built scientific apps for GPU.

## 1. Smart Pointers Everywhere with Explicit `sync()`

**Rule**: Every function that uses data **must** call `sync()` to ensure the data is in the correct memory location.

```fortran
! ❌ Old style - unsafe, location unknown
subroutine compute(x)
    real, pointer :: x(:)
    ! Where is x? HOST? GPU? Nobody knows!
    x(1) = x(1) + 1.0
end subroutine

! ✅ New style - safe, explicit location
subroutine compute(x_buf)
    type(fbuf_type), intent(inout) :: x_buf
    real(real64), pointer :: x(:)
    
    ! Explicitly ensure data is on GPU for computation
    x_buf = sync(x_buf, FBUF_OACC)
    x => get_ptr(x_buf, real64_mold)
    
    !$acc parallel loop present(x)
    do i = 1, size(x)
        x(i) = x(i) + 1.0_real64
    end do
end subroutine
```

This idea is borrowed from Kokkos. Since we are using smart pointers, the sync
call is lazy. If the data is already where it needs to be, no copy happens.
But if we need to move it, we do so. This allows us to encapsulate our
operations better.

## 2. Dictionary Arguments as Python-Style kwargs

**Rule**: All optional parameters go into a dictionary, creating clean, extensible APIs.

```fortran
! ❌ Old style - argument explosion
subroutine solver(A, b, x, max_iter, tol, verbosity, algorithm, &
                 preconditioner, restart_freq, convergence_history)

! ✅ New style - clean kwargs pattern
subroutine solver(A_buf, b_buf, x_buf, kwargs)
    type(fbuf_type), intent(inout) :: A_buf, b_buf, x_buf
    type(dict_type), intent(inout) :: kwargs
    
    ! Extract parameters as needed
    max_iter = dict_get(kwargs, 'max_iterations', int32_mold, found)
    tolerance = dict_get(kwargs, 'tolerance', real64_mold, found)
    algorithm = dict_get(kwargs, 'algorithm', string_mold, found)
    
    ! Store results back in kwargs
    call dict_set(kwargs, 'converged', converged)
    call dict_set(kwargs, 'final_residual', residual_norm)
end subroutine
```

This is borrowed from python's kwargs concept.

## Function Structure Template

Every function follows this pattern:

```fortran
subroutine my_function(data_bufs, kwargs)
    ! 1. Declare fbuf arguments
    type(fbuf_type), intent(inout) :: data_buf1, data_buf2
    type(dict_type), intent(inout) :: kwargs
    
    ! 2. Extract parameters from kwargs
    param1 = dict_get(kwargs, 'param1', real64_mold, found)
    param2 = dict_get(kwargs, 'param2', int32_mold, found)
    
    ! 3. Sync all data to required location
    data_buf1 = sync(data_buf1, FBUF_OACC)  ! GPU computation
    data_buf2 = sync(data_buf2, FBUF_HOST)  ! CPU analysis
    
    ! 4. Get typed pointers
    data1 => get_ptr(data_buf1, real64_mold)
    data2 => get_ptr(data_buf2, real64_mold)
    
    ! 5. Do the work
    ! ... computation here ...
    
    ! 6. Store auxilliary results in kwargs
    call dict_set(kwargs, 'result_status', 'success')
    call dict_set(kwargs, 'final_value', final_result)
end subroutine
```

### Async Programming Pattern

For overlapping computation and I/O:

```fortran
! Start async transfer
analysis_buf = async_start(simulation_buf, FBUF_HOST, queue_id=1)

! Continue GPU computation while transfer happens
call perform_next_timestep(simulation_buf, kwargs)

! sync() automatically waits for async transfer to complete
analysis_buf = sync(analysis_buf, FBUF_HOST)  ! Blocks until async completes
call analyze_on_cpu(analysis_buf, analysis_results)

! Clean up - GPU memory automatically freed
call destroy(analysis_buf)
```

{!README.md!}
