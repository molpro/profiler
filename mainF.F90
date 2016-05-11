PROGRAM mainf
 USE ProfilerF
 IMPLICIT NONE
 TYPE(Profiler) :: p
 INTEGER, PARAMETER :: repeat=20000000
 DOUBLE PRECISION :: a
 INTEGER :: i
 p = Profiler('Fortran')
 CALL p%start('sqrt')
 a=1d0
 DO i=1,repeat
  a=a*SQRT(a+i)/SQRT(a+i+1)
 END DO
 CALL p%stop('sqrt',2*repeat)
 PRINT *,a
 CALL p%start('exp')
 a=1d0
 DO i=1,repeat
  a=a*EXP(a+1d0/i)/EXP(a+1d0/i+1)
 END DO
 PRINT *,a
 CALL p%stop('exp',2*repeat)
 do i=1,1000000
 call p%start('profiler')
  call p%stop('profiler',1)
  end do
 CALL p%print(6)
END PROGRAM mainf
