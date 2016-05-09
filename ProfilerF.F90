!> Framework for timing code sections.
!!
!>Example of use:
!>\code
!! USE ProfilerF
!! IMPLICIT NONE
!! TYPE(Profiler) :: p
!! INTEGER, PARAMETER :: repeat=20000000
!! DOUBLE PRECISION :: a
!! INTEGER :: i
!! p = Profiler('Fortran')
!! CALL p%start('sqrt')
!! a=1d0
!! DO i=1,repeat
!!  a=a*SQRT(a+i)/SQRT(a+i+1)
!! END DO
!! CALL p%stop('sqrt',2*repeat)
!! PRINT *,a
!! CALL p%start('exp')
!! a=1d0
!! DO i=1,repeat
!!  a=a*EXP(a+1d0/i)/EXP(a+1d0/i+1)
!! END DO
!! PRINT *,a
!! CALL p%stop('exp',2*repeat)
!! CALL p%print(6)
!>\endcode

MODULE ProfilerF
 USE iso_c_binding
 IMPLICIT NONE
!> Instances of this type define a single code-timing activity
!! The implementation is via the C binding, which itself makes a C++ object
!! whose address is stored here.
 TYPE :: Profiler
  PRIVATE
  TYPE(c_ptr) :: handle !< C pointer to the corresponding C++ object
 CONTAINS
  PROCEDURE :: start => ProfilerStartF
  PROCEDURE :: declare => ProfilerDeclareF
  PROCEDURE :: stop => ProfilerStopF
  PROCEDURE :: print => ProfilerPrintF
 END TYPE Profiler
 INTERFACE Profiler
  MODULE PROCEDURE ProfilerNewF
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
  SUBROUTINE ProfilerDeclareC(handle, name) BIND (C, name='profilerDeclare')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
  END SUBROUTINE ProfilerDeclareC
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
!> \public Construct a new instance.
!! Should be called through object construction.
  FUNCTION ProfilerNewF(name)
   USE iso_c_binding
   TYPE(Profiler) :: ProfilerNewF
   CHARACTER(len=*), INTENT(in) :: name !< Title of this object
   ProfilerNewF%handle = ProfilerNewC((TRIM(name)//C_NULL_CHAR))
  END FUNCTION ProfilerNewF
!> \public Begin timing a code segment.
!! Should be called through type-bound interface \c start.
  SUBROUTINE ProfilerStartF(this,name)
   CLASS(Profiler), INTENT(in) :: this
   CHARACTER(len=*), INTENT(in) :: name !< name of the code segment
   CHARACTER(kind=c_char,len=1024) :: namecopy
   namecopy=TRIM(name)//C_NULL_CHAR
   CALL ProfilerStartC(this%handle,namecopy)
  END SUBROUTINE ProfilerStartF
!> \public Ensure that a code segment is entered into the result table. This must be called for
!! any code segments for which start/stop is non-collective and therefore might not be called on some processes.
!! Should be called through type-bound interface \c declare.
  SUBROUTINE ProfilerDeclareF(this,name)
   CLASS(Profiler), INTENT(in) :: this
   CHARACTER(len=*), INTENT(in) :: name !< name of the code segment
   CHARACTER(kind=c_char,len=1024) :: namecopy
   namecopy=TRIM(name)//C_NULL_CHAR
   CALL ProfilerDeclareC(this%handle,namecopy)
  END SUBROUTINE ProfilerDeclareF
!> \public End timing a code segment.
!! Should be called through type-bound interface \c stop.
  SUBROUTINE ProfilerStopF(this,name,operations)
   CLASS(Profiler), INTENT(in) :: this
   CHARACTER(len=*), INTENT(in) :: name !< name of the code segment
   INTEGER, INTENT(in) :: operations !< nominal number of operations (or whatever you like) carried out
   CHARACTER(kind=c_char,len=1024) :: namecopy
   INTEGER (kind=c_long) :: operationsC
   namecopy=TRIM(name)//C_NULL_CHAR
   operationsC=INT(operations,kind=C_LONG)
   CALL ProfilerStopC(this%handle,namecopy,operationsC)
  END SUBROUTINE ProfilerStopF
!> \public Print a representation of the object.
!! Should be called through type-bound interface \c print
  SUBROUTINE ProfilerPrintF(this, unit)
   CLASS(Profiler), INTENT(in) :: this
   INTEGER, INTENT(in) :: unit !< Fortran file number; must already be open
   INTEGER(kind=c_int), parameter :: maxResult=102400
   CHARACTER (len=maxResult) :: buffer
   CHARACTER (len=1, kind=c_char), DIMENSION(maxResult) :: result
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
