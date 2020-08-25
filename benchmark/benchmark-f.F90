program test
 use profilerf
#ifdef MOLPRO_PROFILER_MPI
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
 type(profiler) :: p
 integer :: repeat=1000000
 character(256) :: buff='1000000'
 real(4), dimension(2) :: values0, values1
 real(4) :: time0, time1

#ifdef MOLPRO_PROFILER_MPI
    call MPI_Init(c_null_ptr, c_null_ptr)
#endif
    IF (command_argument_count() > 0) CALL get_command_argument(1, buff)
    READ(buff, '(I8)') repeat
    print *, repeat, ' instances'
    p = Profiler('test', 1, 0)
    call p%active(2)
    call etime(values0,time0)
 do i=1,repeat
   call p%start('test')
   call p%stop('test')
 end do
 call p%print(6)
    call etime(values1,time1)
    write (6,*) 'wall time per instance ',(time1-time0)/repeat
    write (6,*) 'user cpu time per instance ',(values1(1)-values0(1))/repeat
    write (6,*) 'system cpu time per instance ',(values1(2)-values0(2))/repeat

    call etime(values0,time0)
!    a=3e29
 do i=1,repeat
   call etime(values1,time1)
!   a=sqrt(a*time1)
   call etime(values1,time1)
 end do
    call etime(values1,time1)
 write (6,*) 'time two instances of etime'!,a
    write (6,*) 'wall time per instance ',(time1-time0)/repeat
    write (6,*) 'user cpu time per instance ',(values1(1)-values0(1))/repeat
    write (6,*) 'system cpu time per instance ',(values1(2)-values0(2))/repeat
#ifdef MOLPRO_PROFILER_MPI
 call MPI_Finalize
#endif
end program test