PROGRAM mainf
    use profilerf
#ifdef HAVE_MPI_H
    use mpi
#endif
    INTEGER :: level, ierr
    CHARACTER(len=8) :: buff='1'
    type(profiler) :: p
#ifdef HAVE_MPI_H
    call MPI_INIT(ierr)
#endif
    IF (command_argument_count().gt.0) CALL get_command_argument(1,buff)
    READ(buff,'(I8)') level
    CALL profiler_module_test(level)

    p = Profiler('testing')
    call p%active(99,stopPrint=0)
    call execute_command_line('sleep 1')
    call p%start('a')
    call execute_command_line('sleep 1')
    call p%start('b')
    call execute_command_line('sleep 1')
    call p%stop('b',1)
    call execute_command_line('sleep 1')
    call p%stop('a',100)
    call p%print(6)
    call p%print(6,cumulative=.true.)
#ifdef HAVE_MPI_H
    call MPI_FINALIZE(ierr)
#endif

END PROGRAM mainf
