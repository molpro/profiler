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
        PROCEDURE :: stop => ProfilerStopF !< End timing a code segment
        PROCEDURE :: active => ProfilerActiveF !< Set the maximum depth at which recording is done
        PROCEDURE :: print => ProfilerPrintF !< \public Print a representation of the object.
    END TYPE Profiler
    INTERFACE Profiler
        MODULE PROCEDURE ProfilerNewF
    END INTERFACE Profiler

    INTERFACE
        !> \private
        FUNCTION ProfilerNewCSerA(name, cpu) BIND (C, name = 'profilerNewSerialA')
            USE iso_c_binding
            CHARACTER(kind = c_char, len = 1), DIMENSION(*), INTENT(in) :: name
            INTEGER(kind = c_int), INTENT(in), VALUE :: cpu
            TYPE(c_ptr) :: ProfilerNewCSerA
        END FUNCTION ProfilerNewCSerA
        !> \private
        FUNCTION ProfilerNewCSerB(name, sort, level, cpu) BIND (C, name = 'profilerNewSerialB')
            USE iso_c_binding
            CHARACTER(kind = c_char, len = 1), DIMENSION(*), INTENT(in) :: name
            INTEGER(kind = c_int), INTENT(in), VALUE :: sort, level, cpu
            TYPE(c_ptr) :: ProfilerNewCSerB
        END FUNCTION ProfilerNewCSerB
#ifdef MOLPRO_PROFILER_MPI
 !> \private
  FUNCTION ProfilerNewCMPIA(name, comm, cpu) BIND (C, name='profilerNewMPIA')
   USE iso_c_binding
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
   INTEGER(kind=c_int), INTENT(in), VALUE ::  comm, cpu
   TYPE(c_ptr) :: ProfilerNewCMPIA
  END FUNCTION ProfilerNewCMPIA
 !> \private
  FUNCTION ProfilerNewCMPIB(name, sort, level, comm, cpu) BIND (C, name='profilerNewMPIB')
   USE iso_c_binding
   CHARACTER(kind=c_char, len=1), DIMENSION(*), INTENT(in) ::  name
   INTEGER(kind=c_int), INTENT(in), VALUE ::  sort, level, comm, cpu
   TYPE(c_ptr) :: ProfilerNewCMPIB
  END FUNCTION ProfilerNewCMPIB
#endif
        !> \private
        SUBROUTINE ProfilerActiveC(handle, level, stopPrint) BIND (C, name = 'profilerActive')
            USE iso_c_binding
            TYPE(c_ptr), INTENT(in), VALUE :: handle
            INTEGER(kind = c_int), INTENT(in), VALUE :: level
            INTEGER(kind = c_int), INTENT(in), VALUE :: stopPrint
        END SUBROUTINE ProfilerActiveC
        !> \private
        SUBROUTINE ProfilerStartC(handle, name) BIND (C, name = 'profilerStart')
            USE iso_c_binding
            TYPE(c_ptr), INTENT(in), VALUE :: handle
            CHARACTER(kind = c_char, len = 1), DIMENSION(*), INTENT(in) :: name
        END SUBROUTINE ProfilerStartC
        !> \private
        SUBROUTINE ProfilerStopC(handle, name, operations) BIND (C, name = 'profilerStop')
            USE iso_c_binding
            TYPE(c_ptr), INTENT(in), VALUE :: handle
            CHARACTER(kind = c_char, len = 1), DIMENSION(*), INTENT(in) :: name
            INTEGER (kind = c_long), INTENT(in), VALUE :: operations
        END SUBROUTINE ProfilerStopC
        !> \private
        SUBROUTINE ProfilerStrC(handle, result, maxResult, verbosity, cumulative, precision) &
                BIND (C, name = 'profilerStrSubroutine')
            USE iso_c_binding
            TYPE(c_ptr), INTENT(in), VALUE :: handle
            CHARACTER(kind = c_char, len = 1), DIMENSION(*), INTENT(inout) :: result
            INTEGER (kind = c_int), INTENT(in), value :: maxResult, verbosity, cumulative, precision
        END SUBROUTINE ProfilerStrC

    END INTERFACE

CONTAINS
    !> \public Construct a new instance.
    !! Should be called through object construction.
    !! @warning If using anything other than the default MPI communicator MPI_COMM_WORLD, for example from use of GA or PPIDD, then you must pass the communicator explicitly.
    !! \param name the title of this object.
    !! \param sort Criterion for sorting printed result table.
    !! \param level
    !! A large value means that data will always be accumulated; zero means that calls to start and stop do nothing.
    !! \param comm The MPI communicator over which statistics should be aggregated.
    !! \param cpu Whether to poll CPU time
    FUNCTION ProfilerNewF(name, sort, level, comm, cpu)
        USE iso_c_binding
        TYPE(Profiler) :: ProfilerNewF
        CHARACTER(len = *), INTENT(in) :: name !< Title of this object
        INTEGER, INTENT(in), OPTIONAL :: sort, level, comm
        LOGICAL, INTENT(in), OPTIONAL :: cpu
        INTEGER(kind = c_int) :: sortC, levelC, commC, cpuC
        IF (PRESENT(sort)) THEN
            sortC = INT(sort, kind = c_int)
        ELSE
            sortC = 0
        ENDIF
        IF (PRESENT(level)) THEN
            levelC = INT(level, kind = c_int)
        ELSE
            levelC = -1
        ENDIF
        cpuC = 0
        IF (PRESENT(cpu)) THEN
            IF (cpu) cpuC = 1
        ENDIF
        IF (PRESENT(sort) .or. PRESENT(level)) THEN
#ifdef MOLPRO_PROFILER_MPI
    IF (PRESENT(comm)) THEN
     ProfilerNewF%handle = ProfilerNewCMPIB((TRIM(name)//C_NULL_CHAR),sortC,levelC,INT(comm,kind=c_int),cpuC)
    ELSE
#endif
            ProfilerNewF%handle = ProfilerNewCSerB((TRIM(name) // C_NULL_CHAR), sortC, levelC,cpuC)
#ifdef MOLPRO_PROFILER_MPI
    ENDIF
#endif
        ELSE
#ifdef MOLPRO_PROFILER_MPI
    IF (PRESENT(comm)) THEN
     ProfilerNewF%handle = ProfilerNewCMPIA((TRIM(name)//C_NULL_CHAR),INT(comm,kind=c_int),cpuC)
    ELSE
#endif
            ProfilerNewF%handle = ProfilerNewCSerA((TRIM(name) // C_NULL_CHAR),cpuC)
#ifdef MOLPRO_PROFILER_MPI
    ENDIF
#endif
        END IF
    END FUNCTION ProfilerNewF
    !> \public Begin timing a code segment.
    !! Should be called through type-bound interface \c start.
    SUBROUTINE ProfilerStartF(this, name)
        CLASS(Profiler), INTENT(in) :: this !< Profiler object
        CHARACTER(len = *), INTENT(in) :: name !< name of the code segment
        CHARACTER(kind = c_char, len = 1024) :: namecopy
        namecopy = TRIM(name) // C_NULL_CHAR
        CALL ProfilerStartC(this%handle, namecopy)
    END SUBROUTINE ProfilerStartF
    !> \public Set the maximum stack depth for which recording will be done
    SUBROUTINE ProfilerActiveF(this, level, stopPrint)
        CLASS(Profiler), INTENT(in) :: this !< Profiler object
        INTEGER, INTENT(in) :: level !< maximum depth at which recording will be done
        INTEGER, INTENT(in), OPTIONAL :: stopPrint !< if non-negative, \c stop prints the statistics since the corresponding \c start
        INTEGER :: stopPrint_
        stopPrint_ = -1; if (present(stopPrint)) stopPrint_ = stopPrint
        CALL ProfilerActiveC(this%handle, INT(level, kind = c_int), INT(stopPrint_, kind = c_int))
    END SUBROUTINE ProfilerActiveF
    !> \public End timing a code segment.
    !! Should be called through type-bound interface \c stop.
    SUBROUTINE ProfilerStopF(this, name, operations)
        CLASS(Profiler), INTENT(in) :: this !< Profiler object
        CHARACTER(len = *), INTENT(in) :: name !< name of the code segment
        INTEGER, INTENT(in), OPTIONAL :: operations !< nominal number of operations (or whatever you like) carried out
        CHARACTER(kind = c_char, len = 1024) :: namecopy
        INTEGER (kind = c_long) :: operationsC
        namecopy = TRIM(name) // C_NULL_CHAR
        IF (PRESENT(operations)) THEN
            operationsC = INT(operations, kind = C_LONG)
        ELSE
            operationsC = INT(0, kind = C_LONG)
        END IF
        CALL ProfilerStopC(this%handle, namecopy, operationsC)
    END SUBROUTINE ProfilerStopF
    !> \public Print a representation of the object.
    !! Should be called through type-bound interface \c print.
    !! Collective across MPI processes.
    SUBROUTINE ProfilerPrintF(this, unit, verbosity, cumulative, precision)
        CLASS(Profiler), INTENT(in) :: this !< Profiler object
        INTEGER, INTENT(in) :: unit !< Fortran file number; must already be open
        INTEGER, INTENT(in), OPTIONAL :: verbosity !< How much to print
        LOGICAL, INTENT(in), OPTIONAL :: cumulative !< Whether local or cumulative resources are printed
        INTEGER, INTENT(in), OPTIONAL :: precision !< Decimal precision
        CHARACTER (len = 1, kind = c_char), DIMENSION(65536) :: result
        INTEGER :: length
        INTEGER(kind = c_int) :: verbosity_, cumulative_, precision_
        verbosity_ = 0; if (present(verbosity)) verbosity_ = int(verbosity, kind = kind(verbosity_))
        cumulative_ = 1; if (present(cumulative)) then; if (.not. cumulative) cumulative_ = 0;
        endif
        precision_ = 3; if (present(precision)) precision_ = int(precision, kind = kind(precision_))
        CALL ProfilerStrC(this%handle, result, int(size(result), kind = c_int), verbosity_, cumulative_, precision_)
        DO length = 1, SIZE(result)
            IF (result(length) == C_NULL_CHAR) EXIT
        END DO
        WRITE (unit, '(65535A)') result(:MIN(length, SIZE(result)) - 1)
    END SUBROUTINE ProfilerPrintF
END MODULE ProfilerF

! outside module to avoid false positives from private module elements
SUBROUTINE profiler_module_test(printlevel)
#ifdef PROFILER_MEMORY
 USE memory
#endif
    USE ProfilerF
    IMPLICIT NONE
    INTEGER, INTENT(in) :: printlevel
    TYPE(Profiler) :: p
    INTEGER, PARAMETER :: repeat = 2000000
    DOUBLE PRECISION :: a
#ifdef PROFILER_MEMORY
 DOUBLE PRECISION, POINTER, DIMENSION(:) :: x
#endif
    INTEGER :: i, kk
    p = Profiler('Fortran')
    !call p%active(2)
    call worker
    call p%start('subtask')
    call worker
    a = 2d0
    do kk = 1, 3
        call p%start('subsubtask')
        a = a + sqrt(1 / a) ! stop the compiler optimising away
        call worker
        call p%stop('subsubtask')
    end do
    call p%stop('subtask')
    if (printlevel > 0) CALL p%print(6)
    if (printlevel > 0) CALL p%print(6, cumulative = .FALSE.)
#ifdef PROFILER_MEMORY
 IF (printlevel > 0) CALL time_memory(1000000*printlevel)
 if (printlevel > 9) PRINT *, 'done',memory_maximum_stack_used(),memory_used('STACK')
#endif
CONTAINS
    SUBROUTINE worker
        CALL p%start('sqrt')
#ifdef PROFILER_MEMORY
 x => memory_allocate(123456)
#endif
        a = 1d0
        DO i = 1, repeat
            a = a * SQRT(a + i) / SQRT(a + i + 1)
        END DO
#ifdef PROFILER_MEMORY
 call memory_release(x)
#endif
        CALL p%stop('sqrt', 2 * repeat)
        IF (printlevel > 99) PRINT *, a ! to avoid compiler optimisation

        ! not explicitly accounted - should appear in total
        a = 1d0
        DO i = 1, repeat
            a = a * EXP(a + 1d0 / i) / EXP(a + 3d0 / i + 1)
        END DO
        IF (printlevel > 99) PRINT *, a ! to avoid compiler optimisation

        CALL p%start('exp')
#ifdef PROFILER_MEMORY
 x => memory_allocate(56789)
 call memory_release(x)
#endif
        a = 1d0
        DO i = 1, repeat
            a = a * EXP(a + 1d0 / i) / EXP(a + 1d0 / i + 1)
        END DO
        IF (printlevel > 99) PRINT *, a ! to avoid compiler optimisation
        CALL p%stop('exp', 2 * repeat)
    END SUBROUTINE worker
#ifdef PROFILER_MEMORY
 SUBROUTINE time_memory(nrep)
  USE profilerF
  USE memory
  INTEGER, INTENT(in) :: nrep
  INTEGER, PARAMETER :: iout=6
  INTEGER :: base
  TYPE(profiler) :: p
  INTEGER, DIMENSION(:),POINTER :: addresses
  INTEGER :: allocation_length
  base = memory_save()
  DO allocation_length=1,0,-1
  WRITE (iout,*) 'Test memory system performance with ',nrep,' calls and allocation length',allocation_length
  p = profiler('Memory system performance')
  addresses => memory_allocate_integer(nrep)
  CALL p%start('icorr')
  DO i=1,nrep
   addresses(i)=icorr(allocation_length)
  END DO
  CALL p%stop('icorr',nrep)
  CALL p%start('print_status')
  CALL memory_print_status(5,'After allocating')
  CALL p%stop('print_status')
  CALL p%start('corlsr')
  PRINT *,addresses(1:5)
  DO i=nrep,1,-1
   CALL corlsr(addresses(i))
  END DO
  call memory_release(addresses)
  CALL p%stop('corlsr',nrep)
  call p%print(iout)
 END DO
  CALL memory_release(base)
 END SUBROUTINE time_memory
#endif
END SUBROUTINE profiler_module_test
