function(add_configure_file LIBRARY_NAME PROJECT_NAME_)
    message(STATUS "add_configure_file ${LIBRARY_NAME}")
    string(TOUPPER ${PROJECT_NAME_} PROJECT_UPPER_NAME)
    target_compile_definitions(${LIBRARY_NAME} PRIVATE NOMAIN)
    if (Molpro_SOURCE_DIR)
        set(MOLPRO 1)
        target_include_directories(${LIBRARY_NAME} PRIVATE "${CMAKE_BINARY_DIR}/src" "${Molpro_SOURCE_DIR}/build" "${Molpro_SOURCE_DIR}/src")
    endif ()
    if (FORTRAN)
        set(${PROJECT_UPPER_NAME}_FORTRAN 1)
        include(CheckFortranCompilerFlag)
        CHECK_Fortran_COMPILER_FLAG("-Wall" _Wallf)
        if (_Wallf)
            set(CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -Wall")
        endif ()
        if (INTEGER8)
            set(${PROJECT_UPPER_NAME}_I8 1)
            foreach (f "-fdefault-integer-8" "-i8")
                CHECK_Fortran_COMPILER_FLAG(${f} _fortran_flags)
                if (_fortran_flags)
                    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${f}")
                endif ()
                unset(_fortran_flags CACHE)
            endforeach ()
        endif ()
    endif ()

    include(CheckCXXCompilerFlag)
    CHECK_CXX_COMPILER_FLAG("-Wall" _Wall)
    if (_Wall)
        set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -Wall")
    endif ()
    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME_}-config.h.in" "
#ifndef ${PROJECT_UPPER_NAME}_CONFIG_H
#define ${PROJECT_UPPER_NAME}_CONFIG_H
#cmakedefine ${PROJECT_UPPER_NAME}_FORTRAN
#cmakedefine ${PROJECT_UPPER_NAME}_I8
#ifndef MOLPRO
#cmakedefine MOLPRO
#endif
#endif
")
    configure_file("${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME_}-config.h.in" ${PROJECT_NAME_}-config.h)
endfunction()
