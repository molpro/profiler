include_guard()
#[=============================================================================[.rst:
LibraryManager
--------------

.. module:: LibraryManager

Overview
^^^^^^^^

Utility functions that make it easier to configure projects as part of software ecosystem

Functions
^^^^^^^^^

#]=============================================================================]
#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_Project

.. code-block:: cmake

    LibraryManager_Project(
                       [MPI_OPTION]
                       [FORTRAN_OPTION]
                       )

Define additional characteristics of the project. Must appear after the CMake ``project()`` command.

``<nameSpace>`` - namespace to be used in the target alias for defined libraries.
Outside users will use the library as ``<nameSpace>::<target>``.

``FORTRAN_OPTION`` if specified defines a CMake ``option(FORTRAN ... ON)``, allowing a user to disable or enable Fortran support and achieve a selective build that includes or omits Fortran components. For this to be effective, Fortran source files should be included into libraries only if ``CMAKE_Fortran_Compile`` is set.
In addition, a CMake ``option(FORTRAN_INTEGER8 ... ON)`` is defined, and if the option is on at run time, Fortran compilation
is carried out with 8-byte integers as the default, and the preprocessor symbol ``FORTRAN_INTEGER8`` is defined.
If using this option, do not previously declare ``Fortran`` as a language in the ``project()`` command or via ``enable_language()``.

``MPI_OPTION``  if specified defines a CMake ``option(MPI ... ON)``, allowing a user to disable MPI support and achieve a serial-only build by querying the variable ``MPI``.
#TODO do we want to configure MPI here, or leave it to the user?
#Could be argument MPI with values ON OFF OPTIONAL?
#Or a second boolean argument MPI? But maybe need to then pass options for FindMPI

As well as processing these options, the function also sets ``CMAKE_PROJECT_VERSION`` and its subcomponent variables using any defined git tags that look like semantic version numbers, and sets up the environment of the ``LibraryManager`` module, so should always be called.

#]=============================================================================]
macro(LibraryManager_Project)
    cmake_parse_arguments("ARG" "MPI_OPTION;FORTRAN_OPTION" "" "" ${ARGN})

    if (ARG_FORTRAN_OPTION)
        option(FORTRAN "Whether to build fortran sources" ON)
        if (FORTRAN)
            enable_language(Fortran)
        endif ()
        message(DEBUG "CMAKE_Fortran_COMPILER ${CMAKE_Fortran_COMPILER}")
    endif ()

    if (ARG_MPI_OPTION)
        option(MPI "Whether to build with MPI" ON)
    endif ()

    _semver()

    message(VERBOSE "PROJECT: ${PROJECT_NAME} ${PROJECT_VERSION}")
    message(DEBUG "PROJECT_VERSION_MAJOR: ${PROJECT_VERSION_MAJOR}")
    message(DEBUG "PROJECT_VERSION_MINOR: ${PROJECT_VERSION_MINOR}")

    if (CMAKE_Fortran_COMPILER)
        option(FORTRAN_INTEGER8 "Whether to build for 64-bit fortran integers" ON)
        if (FORTRAN_INTEGER8)
            include(CheckFortranCompilerFlag)
            foreach (f "-fdefault-integer-8" "-i8")
                CHECK_Fortran_COMPILER_FLAG(${f} _fortran_flags)
                if (_fortran_flags)
                    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${f}")
                endif ()
                unset(_fortran_flags CACHE)
            endforeach ()
            add_compile_definitions(FORTRAN_INTEGER8)
        endif ()
    endif ()

endmacro()
macro(_semver)
    find_package(Git)
    if (Git_FOUND)
        execute_process(
                COMMAND ${GIT_EXECUTABLE} describe --tags --abbrev=0 --always HEAD
                WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
                OUTPUT_STRIP_TRAILING_WHITESPACE
                OUTPUT_VARIABLE PROJECT_VERSION)
    else ()
        set(PROJECT_VERSION "0.0.0")
    endif ()
    string(REGEX REPLACE "^[^0-9]+" "" PROJECT_VERSION "${PROJECT_VERSION}") # strip leading alphabetic
    string(REGEX REPLACE "^.*[^0-9.].*\$" "0.0.0" PROJECT_VERSION "${PROJECT_VERSION}") # bail out if not semver

    string(REGEX REPLACE "([0-9]+)\.([0-9]+)\.([0-9]+)" "\\1" PROJECT_VERSION_MAJOR "${PROJECT_VERSION}")
    string(REGEX REPLACE "([0-9]+)\.([0-9]+)\.([0-9]+)" "\\2" PROJECT_VERSION_MINOR "${PROJECT_VERSION}")
    string(REGEX REPLACE "([0-9]+)\.([0-9]+)\.([0-9]+)" "\\3" PROJECT_VERSION_PATCH "${PROJECT_VERSION}")
    file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/project_version.sh"
            "PROJECT_VERSION=${PROJECT_VERSION}
PROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}
PROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}
PROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}
")
endmacro()
#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_Add

.. code-block:: cmake

    LibraryManager_Add(<target>
                       [INCLUDE_DIR <baseDir>]
                       [NAMESPACE <nameSpace>]
                       [SOURCES <sources> ...]
                       [PUBLIC_HEADER <pubHead> ...]
                       [PRIVATE_HEADER <privHead> ...])

Create a library with specified source files.

``<target>`` - name of the library

``INCLUDE_DIR`` is followed by path to the base directory for reconstructing the source tree.
This is also the include directory of the library as part of build interface.
During installation path to each header will remap ``<baseDir>`` to include directory
(see ``INCLUDE_DIR`` in :cmake:command:`LibraryManager_Install`).
Defaults to ``${CMAKE_CURRENT_SOURCE_DIR}``

``<nameSpace>`` is the name space for the target alias. Outside users will use library as ``<nameSpace>::<target>``.
This should match the value set in :cmake:command:`LibraryManager_Export`.
Default:  no namespacing is used.

``SOURCES`` is followed by a list of source files, which will be appended to
``SOURCES`` property of the library.

``PUBLIC_HEADER`` is followed by a list of header files, which will be appended to
``PUBLIC_HEADER`` property of the library.

``PRIVATE_HEADER`` is followed by a list of header files, which will be appended to `
`PRIVATE_HEADER`` property of the library.

.. note:: Files can be specified as absolute paths, or relative to current source directory,
    but not relative to source directory where ``add_library()`` was called.

#]=============================================================================]
function(LibraryManager_Add target)
    cmake_parse_arguments("ARG" "" "NAMESPACE;INCLUDE_DIR" "" ${ARGN})

    add_library(${target})
    if (DEFINED ARG_NAMESPACE)
        add_library(${ARG_NAMESPACE}::${target} ALIAS ${target})
        set_target_properties(${target} PROPERTIES __LibraryManager_NameSpace "${ARG_NAMESPACE}")
    endif ()

    if (DEFINED ARG_UNPARSED_ARGUMENTS)
        LibraryManager_Append(${target} ${ARG_UNPARSED_ARGUMENTS})
    endif ()

    if (NOT DEFINED ARG_INCLUDE_DIR)
        set(ARG_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR})
    endif ()
    target_include_directories(${target} PUBLIC $<BUILD_INTERFACE:${ARG_INCLUDE_DIR}>)
    set_target_properties(${target} PROPERTIES __LibraryManager_IncludeBaseDir "${ARG_INCLUDE_DIR}")
endfunction()

#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_Append

.. code-block:: cmake

    LibraryManager_Append(<target>
                         [SOURCES <sources> ...]
                         [PUBLIC_HEADER <pubHead> ...]
                         [PRIVATE_HEADER <privHead> ...])

Appends sources and headers to an existing library.

``<target>`` - name of an already existing library

``SOURCES`` is followed by a list of source files, which will be appended to
``SOURCES`` property of the library.

``PUBLIC_HEADER`` is followed by a list of header files, which will be appended to
``PUBLIC_HEADER`` property of the library.

``PRIVATE_HEADER`` is followed by a list of header files, which will be appended to `
`PRIVATE_HEADER`` property of the library.

.. note:: Files can be specified as absolute paths, or relative to current source directory,
    but not relative to source directory where ``add_library()`` was called.

#]=============================================================================]
function(LibraryManager_Append target)
    set(options "")
    set(oneValueArgs "")
    set(multiValueArgs SOURCES PUBLIC_HEADER PRIVATE_HEADER)
    cmake_parse_arguments("ARG" "${options}" "${oneValueArgs}" "${multiValueArgs}" ${ARGN})
    foreach (type SOURCES PUBLIC_HEADER PRIVATE_HEADER)
        foreach (src IN LISTS ARG_${type})
            __LibraryManager_toAbs(${CMAKE_CURRENT_SOURCE_DIR} ${src} srcAbs)
            set_property(TARGET ${target} APPEND PROPERTY ${type} "${srcAbs}")
        endforeach ()
    endforeach ()
    __LibraryManager_fortranInSources(fort ${ARG_SOURCES})
    if (fort)
        # Are this sensible defaults?
        set_target_properties(${target} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran)
        target_include_directories(${target}
                PUBLIC $<INSTALL_INTERFACE:include/fortran>
                $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fortran>)
    endif ()
endfunction()

function(__LibraryManager_fortranInSources out)
    set(${out} OFF PARENT_SCOPE)
    foreach (src IN LISTS ARGN)
        get_filename_component(ext ${src} EXT)
        #todo use regex to match all possible fortran expressions
        if (ext STREQUAL ".F90")
            set(${out} ON PARENT_SCOPE)
        endif ()
    endforeach ()
endfunction()

macro(__LibraryManager_toAbs dir file out)
    set(f "${file}")
    if (NOT IS_ABSOLUTE "${file}")
        set(f "${dir}/${file}")
    endif ()
    set(${out} "${f}")
endmacro()

#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_AppendExternal

.. code-block:: cmake

    LibraryManager_AppendExternal(<target>
                                  PUBLIC_HEADER <pubHead> ...
                                  INCLUDE_DIR <includeDir>)

Appends headers from outside the source tree to an existing library.

``<target>`` - name of an already existing library

``PUBLIC_HEADER`` is followed by a list of header files, which will be appended to
``PUBLIC_HEADER`` property of the library.
They can be absolute paths or relative to ``<includeDir>``.

``INCLUDE_DIR`` is followed by path to the base directory for reconstructing the source tree.
This is also the include directory of the library as part of build interface.
During installation path to each header will remap ``<baseDir>`` to include directory
(see ``INCLUDE_DIR`` in :cmake:command:`LibraryManager_Install`).
During installation headers will be installed relative to the closest base directory
which is part of their path.

.. note:: Files can be specified as absolute paths, or relative to current source directory,
    but not relative to source directory where ``add_library()`` was called.

#]=============================================================================]
function(LibraryManager_AppendExternal target)
    cmake_parse_arguments("ARG" "" "INCLUDE_DIR" "PUBLIC_HEADER" ${ARGN})
    if (NOT DEFINED ARG_INCLUDE_DIR)
        message(FATAL_ERROR "Adding external headers without include base directory")
    else ()
        get_filename_component(ARG_INCLUDE_DIR "${ARG_INCLUDE_DIR}" ABSOLUTE)
    endif ()
    foreach (src IN LISTS ARG_PUBLIC_HEADER)
        get_filename_component(srcAbs "${src}" ABSOLUTE BASE_DIR "${ARG_INCLUDE_DIR}")
        set_property(TARGET ${target} APPEND PROPERTY PUBLIC_HEADER "${srcAbs}")
    endforeach ()
    target_include_directories(${target} PUBLIC $<BUILD_INTERFACE:${ARG_INCLUDE_DIR}>)
    set_property(TARGET ${target} APPEND PROPERTY __LibraryManager_IncludeBaseDir "${ARG_INCLUDE_DIR}")
endfunction()

#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_BLAS

.. code-block:: cmake

    LibraryManager_BLAS(<target> <vendor1> <vendor2>...)

Finds a BLAS library and add it as a requirement for a build target.

``<target>`` - name of an already existing library

<vendor1> <vendor2>... are desired BLAS library vendors, as require for variable ``BLA_VENDOR`` used by CMake ``FindBLAS()``.
Each one is tried in turn until a match is found. If none is matched, then ``FindBLAS()`` is called without vendor specification, to get any available BLAS library.
#]=============================================================================]
function(LibraryManager_BLAS target)
    foreach (BLA_VENDOR ${ARGN})
        message(DEBUG "try BLA_VENDOR ${BLA_VENDOR}")
        if (NOT BLAS_FOUND)
            find_package(BLAS)
            if (BLAS_FOUND)
                message(STATUS "Building with BLAS(${BLA_VENDOR})")
                if (APPLE)
                    target_link_options(${target} PUBLIC "-Wl,-rpath,$ENV{MKLROOT}/lib")
                endif ()
                target_compile_definitions(${target} PUBLIC USE_MKL)
                # Note: lack of include directories is an oversight of CMake and should be fixed soon.
                # See https://gitlab.kitware.com/cmake/cmake/issues/20268
                target_include_directories(${target} PUBLIC $ENV{MKLROOT}/include)
            endif ()
        endif ()
    endforeach ()
    if (NOT BLAS_FOUND)
        unset(BLA_VENDOR)
        find_package(BLAS REQUIRED)
        message(STATUS "Building with BLAS")
    endif ()
    message(DEBUG "BLAS_LIBRARIES ${BLAS_LIBRARIES}")
    target_link_options(${target} PUBLIC ${BLAS_LINKER_FLAGS})
    target_link_libraries(${target} PUBLIC ${BLAS_LIBRARIES})
endfunction()
#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_Install

.. code-block:: cmake

    LibraryManager_Install(<target>
                   [EXPORT <exportSet>]
                   [RUNTIME <runDir>]
                   [LIBRARY <libDir>]
                   [ARCHIVE <archiveDir>]
                   [INCLUDE_DIR <incDir>]
                   [PKG_CONFIG]
                   [<extraArgs> ...])

Installs <target> with public headers mirroing source tree structure.
Also, creates an alias library ``molpro::<target>``.

``EXPORT`` add library to specified export set. A Target in an export set should have the same namespace as all other
targets in the set, or no namespace.

``RUNTIME`` argument passed to install as ``RUNTIME DESTINATION <libDir>``

``LIBRARY`` argument passed to install as ``LIBRARY DESTINATION <libDir>``

``ARCHIVE`` argument passed to install as ``ARCHIVE DESTINATION <libDir>``

``INCLUDE_DIR`` destination for public headers.
This is also the include directory of the library as part of install interface.
The source tree structure is reproduced, matching ``<incDir>`` and include base
directory specified in :cmake:command:`LibraryManager_Add`.
Default value: ``include``

``PKG_CONFIG`` option to create ``<target>.pc`` file for pkg-config.

``<extraArgs>`` are forwarded to install. Note that argument forwarding is quite limited in CMake.

.. note:: Public headers are obtained from ``<target>`` property. If they are not absolute paths, they are assumed
    to be relative to source directory where ``add_library()`` was called. See cmake:command:`LibraryManager_Append`.

#]=============================================================================]
function(LibraryManager_Install target)
    set(oneValueArgs EXPORT RUNTIME LIBRARY ARCHIVE INCLUDE_DIR)
    cmake_parse_arguments("ARG" "PKG_CONFIG" "${oneValueArgs}" "" ${ARGN})
    if (NOT DEFINED ARG_EXPORT)
        set(ARG_EXPORT ${target}-export)
    endif ()
    set(EXPORT "EXPORT;${ARG_EXPORT}")
    get_property(nameSpace TARGET ${target} PROPERTY __LibraryManager_NameSpace)
    if (NOT DEFINED nameSpace)
        set(nameSpace "")
    endif ()
    set(prop __LibraryManager_${ARG_EXPORT}_NameSpace)
    get_property(defined GLOBAL PROPERTY ${prop} DEFINED)
    if (NOT defined AND NOT nameSpace STREQUAL "")
        define_property(GLOBAL PROPERTY ${prop}
                BRIEF_DOCS "Namespace for export set ${ARG_EXPORT}"
                FULL_DOCS "Namespace for export set ${ARG_EXPORT}")
        set_property(GLOBAL PROPERTY ${prop} "${nameSpace}")
    else ()
        get_property(exportNameSpace GLOBAL PROPERTY ${prop})
        if (NOT exportNameSpace STREQUAL nameSpace AND NOT nameSpace STREQUAL "")
            message(FATAL_ERROR "Attempting to install a target into an export-set with incompatible NAMESPACE")
        endif ()
    endif ()
    foreach (v RUNTIME LIBRARY ARCHIVE)
        set(${v} "")
        if (ARG_${v})
            set(${v} "${v};DESTINATION;${ARG_${v}}")
        endif ()
    endforeach ()
    if (NOT DEFINED ARG_INCLUDE_DIR)
        set(ARG_INCLUDE_DIR include)
    endif ()
    target_include_directories(${target} PUBLIC $<INSTALL_INTERFACE:${ARG_INCLUDE_DIR}>)

    # For now this is the best I can do. Private headers are never used.
    get_property(pubHead TARGET ${target} PROPERTY PUBLIC_HEADER)
    get_property(privHead TARGET ${target} PROPERTY PRIVATE_HEADER)
    set_property(TARGET ${target} PROPERTY PUBLIC_HEADER "")
    set_property(TARGET ${target} PROPERTY PRIVATE_HEADER "")
    install(TARGETS "${target}" ${EXPORT} ${RUNTIME} ${LIBRARY} ${ARCHIVE} ${ARG_UNPARSED_ARGUMENTS})
    set_property(TARGET ${target} PROPERTY PUBLIC_HEADER "${pubHead}")
    set_property(TARGET ${target} PROPERTY PRIVATE_HEADER "${privHead}")

    # Install public headers one at a time
    get_property(srcDir TARGET ${target} PROPERTY SOURCE_DIR)
    get_property(baseDirs TARGET ${target} PROPERTY __LibraryManager_IncludeBaseDir)
    foreach (src IN LISTS pubHead)
        get_filename_component(src "${src}" ABSOLUTE BASE_DIR "${srcDir}")
        __LibraryManager_chooseBaseDir("${src}" "${baseDirs}" baseDir)
        file(RELATIVE_PATH path "${baseDir}" "${src}")
        get_filename_component(path "${path}" DIRECTORY)
        install(FILES ${src} DESTINATION ${ARG_INCLUDE_DIR}/${path})
    endforeach ()

    # install fortran module directory
    get_property(path TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY)
    if (NOT "${path}" STREQUAL "")
        get_property(path TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY)
        install(DIRECTORY ${path} DESTINATION include OPTIONAL)
    endif ()

    if (ARG_PKG_CONFIG)
        __LibraryManager_writePkgConfig(${target} content)
        #        message("content=${content}")
        file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${target}.pc CONTENT "${content}")
        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${target}.pc DESTINATION ${CMAKE_INSTALL_PREFIX}/lib/pkgconfig)
    endif ()

    # -config configuration script support TODO
endfunction()

# Chooses the base directory closest to the source file
function(__LibraryManager_chooseBaseDir src baseDirs out)
    set(matched "")
    foreach (dir IN LISTS baseDirs)
        if ("${src}" MATCHES "^${dir}")
            list(APPEND matched "${dir}")
        endif ()
    endforeach ()
    if (matched STREQUAL "")
        message(FATAL_ERROR
                "no matching base directories for src=${src}. Make sure all headers were added with LibraryManager.")
    endif ()
    set(baseDir)
    set(n -1)
    foreach (path IN LISTS matched)
        string(REPLACE "/" ";" dirs "${path}")
        list(LENGTH dirs i)
        if ("${i}" GREATER "${n}")
            set(baseDir "${path}")
            set(n "${i}")
        endif ()
    endforeach ()
    set("${out}" "${baseDir}" PARENT_SCOPE)
endfunction()

# Updates properties recursively
macro(__LibraryManager_appendProps target)
    get_property(compDef TARGET ${target} PROPERTY INTERFACE_COMPILE_DEFINITIONS)
    get_property(compOpt TARGET ${target} PROPERTY INTERFACE_COMPILE_OPTIONS)
    get_property(inclDir TARGET ${target} PROPERTY INTERFACE_INCLUDE_DIRECTORIES)
    get_property(linkLib TARGET ${target} PROPERTY INTERFACE_LINK_LIBRARIES)
    get_property(linkOpt TARGET ${target} PROPERTY INTERFACE_LINK_OPTIONS)
    list(APPEND compDefRec ${compDef})
    list(APPEND compOptRec ${compOpt})
    list(APPEND inclDirRec ${inclDir})
    list(APPEND linkOptRec ${linkOpt})
    get_target_property(type ${target} TYPE)
    get_target_property(name ${target} NAME)
    if (type STREQUAL "STATIC_LIBRARY")
        list(PREPEND linkLib "${CMAKE_INSTALL_PREFIX}/lib/lib${name}.a")
    elseif (type STREQUAL "SHARED_LIBRARY" OR type STREQUAL "MODULE_LIBRARY")
        list(PREPEND linkLib "${CMAKE_INSTALL_PREFIX}/lib/lib${name}.so")
    endif ()
    #    message("compDef=${compDef}")
    #    message("compOpt=${compOpt}")
    #    message("inclDir=${inclDir}")
    #    message("linkLib=${linkLib}")
    #    message("linkOpt=${linkOpt}")
    foreach (lib IN LISTS linkLib)
        string(REGEX REPLACE "::@<.*>" "" lib "${lib}")
        if (TARGET ${lib})
            __LibraryManager_appendProps(${lib})
        else ()
            list(APPEND linkLibRec "${lib}")
        endif ()
    endforeach ()
endmacro()

macro(__LibraryManager_parseDefs)
    foreach (v compOptRec compDefRec)
        foreach (x IN LISTS ${v})
            if ("${x}" MATCHES "^SHELL:")
                string(REPLACE "SHELL:" "" x "${x}")
                separate_arguments(x UNIX_COMMAND "${x}")
            endif ()
            if (NOT "${x}" MATCHES "^-D")
                set(x "-D${x}")
            endif ()
            list(APPEND ${v}_copy "${x}")
        endforeach ()
        set(${v} "${${v}_copy}")
    endforeach ()
endmacro()

macro(__LibraryManager_parseLinkOpt)
    list(JOIN CMAKE_CXX_LINKER_WRAPPER_FLAG " " linkWrapFlag)
    foreach (v linkOptRec)
        foreach (x IN LISTS ${v})
            if ("${x}" MATCHES "^SHELL:")
                string(REPLACE "SHELL:" "" x "${x}")
                separate_arguments(x UNIX_COMMAND "${x}")
            endif ()
            set(sep ",")
            if ("${x}" MATCHES "LINKER:SHELL:")
                set(sep " ")
                string(REPLACE "LINKER:SHELL:" "LINKER:" x "${x}")
            endif ()
            if ("${x}" MATCHES "LINKER:")
                string(REPLACE "LINKER:" "${linkWrapFlag}" x "${x}")
                string(REPLACE "${sep}" "${CMAKE_CXX_LINKER_WRAPPER_FLAG_SEP}" x "${x}")
            endif ()
            list(APPEND ${v}_copy "${x}")
        endforeach ()
        set(${v} "${${v}_copy}")
    endforeach ()
endmacro()

macro(__LibraryManager_parseInclDir)
    foreach (x IN LISTS inclDirRec)
        if ("${x}" MATCHES "[$]<INSTALL_INTERFACE:.*>")
            string(REGEX MATCH "[$]<INSTALL_INTERFACE:(.*)>" path "${x}")
            set(path "${CMAKE_MATCH_1}")
            if (NOT IS_ABSOLUTE "${path}" AND NOT "${path}" STREQUAL "")
                set(path "${CMAKE_INSTALL_PREFIX}/${path}")
                set(x "$<INSTALL_INTERFACE:${path}>")
            endif ()
        endif ()
        list(APPEND inclDirRec-copy "${x}")
    endforeach ()
    set(inclDirRec "${inclDirRec-copy}")
endmacro()

# options we might still be missing
# CMAKE_EXE_LINKER_FLAGS
# CMAKE_EXE_LINKER_FLAGS_${CMAKE_BUILD_TYPE}
function(__LibraryManager_writePkgConfig target out)
    set(content "")
    string(TOUPPER "${CMAKE_BUILD_TYPE}" bt)
    set(Cflags ${CMAKE_CXX_FLAGS} ${CMAKE_CXX_FLAGS_${bt}})
    #    message("Cflags=${Cflags}")
    __LibraryManager_appendProps(${target})
    __LibraryManager_parseDefs()
    __LibraryManager_parseInclDir()
    __LibraryManager_parseLinkOpt()
    #    message("compDefRec=${compDefRec}")
    #    message("compOptRec=${compOptRec}")
    #    message("inclDirRec=${inclDirRec}")
    #    message("linkLibRec=${linkLibRec}")
    #    message("linkOptRec=${linkOptRec}")
    list(JOIN inclDirRec " -I" inclDirRec)
    list(JOIN compOptRec " " compOptRec)
    list(JOIN compDefRec " " compDefRec)
    list(JOIN linkOptRec " " linkOptRec)
    list(JOIN linkLibRec " " linkLibRec)
    set(Cflags "${Cflags} ${compOptRec} ${compDefRec} -I${CMAKE_INSTALL_PREFIX}/include -I${inclDirRec}")
    set(libs "${linkOptRec} ${linkLibRec}")

    set(content
            "
Name: ${target}
Description: Library ${target} built with CMake and installed with LibraryManager
Version: ${PROJECT_VERSION}
Requires:
Requires.private:
Cflags: ${Cflags}
Libs: ${libs}
Libs.private:
")
    # Use this hack for now: https://cmake.org/pipermail/cmake/2017-May/065529.html
    string(REPLACE "$<BUILD_INTERFACE:" "$<0:" content "${content}")
    string(REPLACE "$<INSTALL_INTERFACE:" "$<1:" content "${content}")
    string(REPLACE "$<INSTALL_PREFIX>" "${CMAKE_INSTALL_PREFIX}" content "${content}")
    set(${out} "${content}" PARENT_SCOPE)
endfunction()

#[=============================================================================[.rst:
.. cmake:command:: LibraryManager_Export

.. code-block:: cmake

    LibraryManager_Export(<projectName>
                   [EXPORT <exportSet>]
                   [FILE <runDir>]
                   [NAMESPACE <libDir>]
                   [DESTINATION <archiveDir>])

Installs export set, creating ``<projectName>Config.cmake`` file.
Meaning of options is the same as in ``install(EXPORT)``,
but in most cases correct values are set based on ``<projectName>``.
If ``NAMESPACE`` is not specified than the same one specified in :cmake:command:`LibraryManager_Add`
will be used.

.. note:: ``NAMESPACE`` will be appended with ``::`` and in most situations they should
          not be specified explicitly
#]=============================================================================]
function(LibraryManager_Export project)
    set(oneValueArgs EXPORT FILE NAMESPACE DESTINATION)
    cmake_parse_arguments("ARG" "" "${oneValueArgs}" "" ${ARGN})

    if (NOT DEFINED ARG_EXPORT)
        set(ARG_EXPORT ${project}-export)
    endif ()
    if (NOT DEFINED ARG_FILE)
        set(ARG_FILE ${project}Config.cmake)
    endif ()
    if (NOT DEFINED ARG_NAMESPACE)
        set(prop __LibraryManager_${ARG_EXPORT}_NameSpace)
        get_property(defined GLOBAL PROPERTY ${prop} DEFINED)
        set(nameSpace "")
        if (defined)
            get_property(nameSpace GLOBAL PROPERTY ${prop})
        endif ()
    else ()
        set(nameSpace "${ARG_NAMESPACE}")
    endif ()
    set(ARG_NAMESPACE "")
    if (NOT nameSpace STREQUAL "")
        if (NOT nameSpace MATCHES ".*::$")
            set(ARG_NAMESPACE "NAMESPACE;${nameSpace}::")
        else ()
            set(ARG_NAMESPACE "NAMESPACE;${nameSpace}::")
        endif ()
    endif ()
    if (NOT DEFINED ARG_DESTINATION)
        set(ARG_DESTINATION lib/cmake/${nameSpace}/${project})
    endif ()

    #    message("ARG_EXPORT=${ARG_EXPORT}")
    #    message("ARG_FILE=${ARG_FILE}")
    #    message("ARG_DESTINATION=${ARG_DESTINATION}")
    #    message("ARG_NAMESPACE=${ARG_NAMESPACE}")
    install(
            EXPORT ${ARG_EXPORT}
            FILE ${ARG_FILE}
            DESTINATION ${ARG_DESTINATION}
            ${ARG_NAMESPACE}
    )
endfunction()
