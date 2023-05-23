PROGRAM mainf
!call example()
!contains
!subroutine example()
    use profilerf
#ifdef HAVE_MPI_H
  !include 'mpif.h'
  USE iso_c_binding
    INTERFACE
    SUBROUTINE MPI_Init (argc, argv) BIND(C, name = 'MPI_Init')
      USE ISO_C_BINDING
      TYPE(C_PTR), INTENT(IN) :: argc,argv
    END SUBROUTINE MPI_INIT
    SUBROUTINE MPI_FINALIZE() BIND(C, name = 'MPI_Finalize')
    END SUBROUTINE MPI_FINALIZE
  END INTERFACE
#endif
    INTEGER :: level, ierr, repeat
    CHARACTER(len = 8) :: buff = '1'
    type(profiler) :: p, pcpu
#ifdef HAVE_MPI_H
    call MPI_Init(c_null_ptr, c_null_ptr)
    !call MPI_INIT(ierr)
#endif
    IF (command_argument_count() > 0) CALL get_command_argument(1, buff)
    READ(buff, '(I8)') level
    CALL profiler_module_test(level)

    !p = Profiler('testing',1,0,MPI_COMM_WORLD)
    call p%construct('testing', 1, 0)
    call p%active(99, stopPrint = 0)
    call execute_command_line('sleep 1')
    call p%start('a')
    call execute_command_line('sleep 1')
    call p%start('b')
    call execute_command_line('sleep 1')
    call p%stop('b', 1)
    call execute_command_line('sleep 1')
    call p%stop('a', 100)
    repeat=100000
    call p%start('many wall')
    do i=1,repeat
        call p%start('wall')
        call p%stop('wall')
    end do
    call p%stop('many wall', repeat)
    call p%start('many wall plus fortran cpu_time')
    do i=1,repeat
        call p%start('wall plus fortran cpu_time')
        call cpu_time(t)
        call p%stop('wall plus fortran cpu_time')
    end do
    call p%stop('many wall plus fortran cpu_time', repeat)
    call p%start('many fortran cpu_time')
    do i=1,repeat
        call cpu_time(t)
    end do
    call p%stop('many fortran cpu_time', repeat)
    call p%print(6)
    call p%print(6, cumulative = .false.)
    call p%dotgraph('profiler-fortran-example.gv')
    call p%destroy() ! This has to be called only if p is declared in the main program, because in that case the final subroutine of Profiler is not invoked
    call pcpu%construct('wall+cpu', 1, 0, cpu=.true.)
    call pcpu%active(99, stopPrint = 0)
    call pcpu%start('many wall+cpu')
    do i=1,repeat
        call pcpu%start('wall+cpu')
        call pcpu%stop('wall+cpu')
    end do
    call pcpu%stop('many wall+cpu', repeat)
    call pcpu%start('many timing_molpro')
    do i=1,repeat
        call TIMING_MOLPRO(cpu, sys, wall)
    end do
    call pcpu%stop('many timing_molpro', repeat)
    call pcpu%start('many walcl')
    do i=1,repeat
        a=wallcl()
    end do
    call pcpu%stop('many walcl', repeat)
    call pcpu%print(6)
    call pcpu%destroy() ! This has to be called only if p is declared in the main program, because in that case the final subroutine of Profiler is not invoked
#ifdef HAVE_MPI_H
  call MPI_Finalize()
  !call MPI_FINALIZE(ierr)
#endif
!end subroutine example
END PROGRAM mainf
