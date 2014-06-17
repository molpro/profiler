MODULE ProfilerF
 USE iso_c_binding
 IMPLICIT NONE
 TYPE :: Profiler
  TYPE(c_ptr) :: handle
 CONTAINS
  PROCEDURE :: start => ProfilerStartF
  PROCEDURE :: stop => ProfilerStopF
  PROCEDURE :: print => ProfilerPrintF
 END TYPE Profiler
 INTERFACE Profiler
  MODULE PROCEDURE :: ProfilerNewF
 END INTERFACE Profiler

 INTERFACE
  FUNCTION ProfilerNewC(name) BIND (C, name='profilerNew')
   USE iso_c_binding
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
   TYPE(c_ptr) :: ProfilerNewC
  END FUNCTION ProfilerNewC
  SUBROUTINE ProfilerStartC(handle, name) BIND (C, name='profilerStart')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
  END SUBROUTINE ProfilerStartC
  SUBROUTINE ProfilerStopC(handle, name, operations) BIND (C, name='profilerStop')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
   INTEGER (kind=c_long), INTENT(in), VALUE :: operations
  END SUBROUTINE ProfilerStopC
  SUBROUTINE ProfilerStrC(handle, result, maxResult) BIND (C, name='profilerStrSubroutine')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(inout) ::  result
   INTEGER (kind=c_int), INTENT(in), value :: maxResult
  END SUBROUTINE ProfilerStrC

 END INTERFACE
   
 CONTAINS
  FUNCTION ProfilerNewF(name)
   USE iso_c_binding
   TYPE(Profiler) :: ProfilerNewF
   CHARACTER(len=*), INTENT(in) :: name
   ProfilerNewF%handle = ProfilerNewC((TRIM(name)//C_NULL_CHAR))
  END FUNCTION ProfilerNewF
  SUBROUTINE ProfilerStartF(this,name)
   CLASS(Profiler), INTENT(in) :: this
   CHARACTER(len=*), INTENT(in) :: name
   CHARACTER(kind=c_char,len=1024) :: namecopy
   namecopy=TRIM(name)//C_NULL_CHAR
   CALL ProfilerStartC(this%handle,namecopy)
  END SUBROUTINE ProfilerStartF
  SUBROUTINE ProfilerStopF(this,name,operations)
   CLASS(Profiler), INTENT(in) :: this
   CHARACTER(len=*), INTENT(in) :: name
   INTEGER, INTENT(in) :: operations
   CHARACTER(kind=c_char,len=1024) :: namecopy
   INTEGER (kind=c_long) :: operationsC
   namecopy=TRIM(name)//C_NULL_CHAR
   operationsC=INT(operations,kind=C_LONG)
   CALL ProfilerStopC(this%handle,namecopy,operationsC)
  END SUBROUTINE ProfilerStopF
  SUBROUTINE ProfilerPrintF(this, unit)
   CLASS(Profiler), INTENT(in) :: this
   INTEGER, INTENT(in) :: unit
   INTEGER(kind=c_int), parameter :: maxResult=102400
   CHARACTER (len=maxResult) :: buffer
   CHARACTER (len=1, kind=c_char), DIMENSION(maxResult) :: result
   byte, POINTER, DIMENSION(:) :: bytearray
   CHARACTER :: c
   INTEGER :: i,length
   CALL ProfilerStrC(this%handle,result,maxResult)
   DO length=1,maxResult
    IF (result(length).EQ.C_NULL_CHAR) EXIT
   END DO
   length=length-1
   DO i=1,length
    buffer(i:i)=result(i)
   END DO
   WRITE (unit,'(A)') buffer
  END SUBROUTINE ProfilerPrintF
 END MODULE ProfilerF
