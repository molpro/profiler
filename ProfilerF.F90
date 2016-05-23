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
  PROCEDURE :: start => ProfilerStartF !< Begin timing a code segment
!> \public Ensure that a code segment is entered into the result table. This must be called for
!! any code segments for which start/stop is non-collective and therefore might not be called on some processes.
  PROCEDURE :: declare => ProfilerDeclareF
  PROCEDURE :: stop => ProfilerStopF !< End timing a code segment
  PROCEDURE :: active => ProfilerActiveF !< Set the maximum depth at which recording is done
  PROCEDURE :: print => ProfilerPrintF !< \public Print a representation of the object.
 END TYPE Profiler
 INTERFACE Profiler
  MODULE PROCEDURE ProfilerNewF
 END INTERFACE Profiler

 INTERFACE
 !> \private
  FUNCTION ProfilerNewC(name) BIND (C, name='profilerNew')
   USE iso_c_binding
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
   TYPE(c_ptr) :: ProfilerNewC
  END FUNCTION ProfilerNewC
 !> \private
  SUBROUTINE ProfilerActiveC(handle, level, stopPrint) BIND (C, name='profilerActive')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   INTEGER(kind=c_int), INTENT(in), VALUE ::  level
   INTEGER(kind=c_int), INTENT(in), VALUE ::  stopPrint
  END SUBROUTINE ProfilerActiveC
 !> \private
  SUBROUTINE ProfilerStartC(handle, name) BIND (C, name='profilerStart')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
  END SUBROUTINE ProfilerStartC
 !> \private
  SUBROUTINE ProfilerDeclareC(handle, name) BIND (C, name='profilerDeclare')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
  END SUBROUTINE ProfilerDeclareC
 !> \private
  SUBROUTINE ProfilerStopC(handle, name, operations) BIND (C, name='profilerStop')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
   INTEGER (kind=c_long), INTENT(in), VALUE :: operations
  END SUBROUTINE ProfilerStopC
 !> \private
  SUBROUTINE ProfilerStrC(handle, result, maxResult, verbosity, cumulative, precision) BIND (C, name='profilerStrSubroutine')
   USE iso_c_binding
   TYPE(c_ptr), INTENT(in), VALUE :: handle
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(inout) ::  result
   INTEGER (kind=c_int), INTENT(in), value :: maxResult, verbosity, cumulative, precision
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
   CLASS(Profiler), INTENT(in) :: this !< Profiler object
   CHARACTER(len=*), INTENT(in) :: name !< name of the code segment
   CHARACTER(kind=c_char,len=1024) :: namecopy
   namecopy=TRIM(name)//C_NULL_CHAR
   CALL ProfilerStartC(this%handle,namecopy)
  END SUBROUTINE ProfilerStartF
!> \public Set the maximum stack depth for which recording will be done
  SUBROUTINE ProfilerActiveF(this,level,stopPrint)
   CLASS(Profiler), INTENT(in) :: this !< Profiler object
   INTEGER, INTENT(in) :: level !< maximum depth at which recording will be done
   INTEGER, INTENT(in), OPTIONAL :: stopPrint !< if non-negative, \c stop prints the statistics since the corresponding \c start
   INTEGER :: stopPrint_
   stopPrint_=-1; if (present(stopPrint)) stopPrint_=stopPrint
   CALL ProfilerActiveC(this%handle,INT(level,kind=c_int),INT(stopPrint_,kind=c_int))
  END SUBROUTINE ProfilerActiveF
!> \public Ensure that a code segment is entered into the result table. This must be called for
!! any code segments for which start/stop is non-collective and therefore might not be called on some processes.
!! Should be called through type-bound interface \c declare.
  SUBROUTINE ProfilerDeclareF(this,name)
   CLASS(Profiler), INTENT(in) :: this !< Profiler object
   CHARACTER(len=*), INTENT(in) :: name !< name of the code segment
   CHARACTER(kind=c_char,len=1024) :: namecopy
   namecopy=TRIM(name)//C_NULL_CHAR
   CALL ProfilerDeclareC(this%handle,namecopy)
  END SUBROUTINE ProfilerDeclareF
!> \public End timing a code segment.
!! Should be called through type-bound interface \c stop.
  SUBROUTINE ProfilerStopF(this,name,operations)
   CLASS(Profiler), INTENT(in) :: this !< Profiler object
   CHARACTER(len=*), INTENT(in) :: name !< name of the code segment
   INTEGER, INTENT(in), OPTIONAL :: operations !< nominal number of operations (or whatever you like) carried out
   CHARACTER(kind=c_char,len=1024) :: namecopy
   INTEGER (kind=c_long) :: operationsC
   namecopy=TRIM(name)//C_NULL_CHAR
   IF (PRESENT(operations)) THEN
    operationsC=INT(operations,kind=C_LONG)
   ELSE
    operationsC=INT(0,kind=C_LONG)
   END IF
   CALL ProfilerStopC(this%handle,namecopy,operationsC)
  END SUBROUTINE ProfilerStopF
!> \public Print a representation of the object.
!! Should be called through type-bound interface \c print
  SUBROUTINE ProfilerPrintF(this, unit, verbosity, cumulative, precision)
   CLASS(Profiler), INTENT(in) :: this !< Profiler object
   INTEGER, INTENT(in) :: unit !< Fortran file number; must already be open
   INTEGER, INTENT(in), OPTIONAL :: verbosity !< How much to print
   LOGICAL, INTENT(in), OPTIONAL :: cumulative !< Whether local or cumulative resources are printed
   INTEGER, INTENT(in), OPTIONAL :: precision !< Decimal precision
   CHARACTER (len=1, kind=c_char), DIMENSION(65536) :: result
   INTEGER :: length
   INTEGER(kind=c_int) :: verbosity_, cumulative_, precision_
   verbosity_=0; if (present(verbosity)) verbosity_=int(verbosity,kind=kind(verbosity_))
   cumulative_=0; if (present(cumulative)) then; if (cumulative) cumulative_=1; endif
   precision_=3; if (present(precision)) precision_=int(precision,kind=kind(precision_))
   CALL ProfilerStrC(this%handle,result,int(size(result),kind=c_int),verbosity_,cumulative_,precision_)
   DO length=1,SIZE(result)
    IF (result(length).EQ.C_NULL_CHAR) EXIT
   END DO
   WRITE (unit,'(65535A)') result(:MIN(length,SIZE(result))-1)
  END SUBROUTINE ProfilerPrintF
 END MODULE ProfilerF

#ifdef MOLPRO
#define MEMORY
#endif
! outside module to avoid false positives from private module elements
SUBROUTINE profiler_module_test(printlevel)
#ifdef MEMORY
 USE memory
#endif
 USE ProfilerF
 IMPLICIT NONE
 INTEGER, INTENT(in) :: printlevel
 TYPE(Profiler) :: p
 INTEGER, PARAMETER :: repeat=2000000
 DOUBLE PRECISION :: a
 DOUBLE PRECISION, POINTER, DIMENSION(:) :: x
 INTEGER :: i,kk
 p = Profiler('Fortran')
 !call p%active(2)
 call worker
 call p%start('subtask')
 call worker
 a=2d0
 do kk=1,3
 call p%start('subsubtask')
 a=a+sqrt(1/a) ! stop the compiler optimising away
 call worker
 call p%stop('subsubtask')
 end do
 call p%stop('subtask')
 if (printlevel.gt.0) CALL p%print(6)
 if (printlevel.gt.0) CALL p%print(6,cumulative=.TRUE.)
#ifdef MEMORY
 if (printlevel.gt.9) PRINT *, 'done',memory_used('STACK',.TRUE.),memory_used('STACK',.FALSE.)
#endif
 CONTAINS
 SUBROUTINE worker
 CALL p%start('sqrt')
#ifdef MEMORY
 x => memory_allocate(123456)
#endif
 a=1d0
 DO i=1,repeat
  a=a*SQRT(a+i)/SQRT(a+i+1)
 END DO
#ifdef MEMORY
 call memory_release(x)
#endif
 CALL p%stop('sqrt',2*repeat)
 IF (printlevel.GT.99) PRINT *,a ! to avoid compiler optimisation

! not explicitly accounted - should appear in total
 a=1d0
 DO i=1,repeat
  a=a*EXP(a+1d0/i)/EXP(a+3d0/i+1)
 END DO
 IF (printlevel.GT.99) PRINT *,a ! to avoid compiler optimisation

 CALL p%start('exp')
#ifdef MEMORY
 x => memory_allocate(56789)
 call memory_release(x)
#endif
 a=1d0
 DO i=1,repeat
  a=a*EXP(a+1d0/i)/EXP(a+1d0/i+1)
 END DO
 IF (printlevel.GT.99) PRINT *,a ! to avoid compiler optimisation
 CALL p%stop('exp',2*repeat)
 END SUBROUTINE worker
END SUBROUTINE profiler_module_test

#ifdef MAIN
PROGRAM mainf
 use profilerf
 INTEGER :: level
 CHARACTER(len=8) :: buff='1'
 type(profiler) :: p
 IF (command_argument_count().gt.0) CALL get_command_argument(1,buff)
 READ(buff,'(I)') level
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

END PROGRAM mainf
#endif
