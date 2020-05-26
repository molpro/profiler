include_guard()

#[=============================================================================[.rst
LibraryManager
------

.. module:: LibraryManager

Overview
^^^^^^^^

Utility functions that make it easier to configure projects as part of software ecosystem

Functions
^^^^^^^^^

#]=============================================================================]

#[=============================================================================[.rst
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

#[=============================================================================[.rst
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

#[=============================================================================[.rst
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

``EXPORT`` add library to specified export set.

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
    get_property(baseDir TARGET ${target} PROPERTY __LibraryManager_IncludeBaseDir)
    foreach (src IN LISTS pubHead)
        __LibraryManager_toAbs("${srcDIr}" "${src}" src)
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
        message("content=${content}")
        file(GENERATE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${target}.pc CONTENT "${content}")
        install(FILES ${CMAKE_CURRENT_BINARY_DIR}/${target}.pc DESTINATION ${CMAKE_INSTALL_PREFIX}/lib)
    endif ()

    # -config configuration script support TODO
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
    message("compDef=${compDef}")
    message("compOpt=${compOpt}")
    message("inclDir=${inclDir}")
    message("linkLib=${linkLib}")
    message("linkOpt=${linkOpt}")
    foreach (lib IN LISTS linkLibRec)
        if (TARGET "${lib}")
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
    message("Cflags=${Cflags}")
    __LibraryManager_appendProps(${target})
    __LibraryManager_parseDefs()
    __LibraryManager_parseInclDir()
    __LibraryManager_parseLinkOpt()
    #    message("compDefRec=${compDefRec}")
    #    message("compOptRec=${compOptRec}")
    #    message("inclDirRec=${inclDirRec}")
    #    message("linkLibRec=${linkLibRec}")
    #    message("linkOptRec=${linkOptRec}")
    list(JOIN inclDirRec " -I " inclDirRec)
    list(JOIN compOptRec " " compOptRec)
    list(JOIN compDefRec " " compDefRec)
    list(JOIN linkOptRec " " linkOptRec)
    list(JOIN linkLibRec " " linkLibRec)
    set(Cflags "${Cflags} ${compOptRec} ${compDefRec} -I ${inclDirRec}")
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

#[=============================================================================[.rst
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
        set(nameSpace "")
        set(ARG_NAMESPACE "")
    else ()
        set(nameSpace "${ARG_NAMESPACE}")
        set(ARG_NAMESPACE "NAMESPACE;${ARG_NAMESPACE}")
    endif ()
    if (NOT DEFINED ARG_DESTINATION)
        set(ARG_DESTINATION lib/cmake/${nameSpace}/${project})
    endif ()

    install(
            EXPORT ${ARG_EXPORT}
            FILE ${ARG_FILE}
            DESTINATION ${ARG_DESTINATION}
            ${ARG_NAMESPACE}
    )
endfunction()
