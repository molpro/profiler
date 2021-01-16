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
    INTEGER :: level, ierr
    CHARACTER(len = 8) :: buff = '1'
    type(profiler) :: p
#ifdef HAVE_MPI_H
    call MPI_Init(c_null_ptr, c_null_ptr)
    !call MPI_INIT(ierr)
#endif
    IF (command_argument_count() > 0) CALL get_command_argument(1, buff)
    READ(buff, '(I8)') level
    CALL profiler_module_test(level)

    !p = Profiler('testing',1,0,MPI_COMM_WORLD)
    p = Profiler('testing', 1, 0)
    call p%active(99, stopPrint = 0)
    call execute_command_line('sleep 1')
    call p%start('a')
    call execute_command_line('sleep 1')
    call p%start('b')
    call execute_command_line('sleep 1')
    call p%stop('b', 1)
    call execute_command_line('sleep 1')
    call p%stop('a', 100)
    call p%print(6)
    call p%print(6, cumulative = .false.)
    call p%destroy()
#ifdef HAVE_MPI_H
  call MPI_Finalize()
  !call MPI_FINALIZE(ierr)
#endif
!end subroutine example
END PROGRAM mainf
