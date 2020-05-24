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
                       [INCLUDE_CURRENT]
                       [NAMESPACE <nameSpace>]
                      [SOURCES <sources> ...]
                      [PUBLIC_HEADER <pubHead> ...]
                      [PRIVATE_HEADER <privHead> ...])

Create a library with specified source files.

``<target>`` - name of the library

``INCLUDE_CURRENT`` option if specified sets current directory to be the include directory.
When installing public headers, structure of the source tree is recreated relative to the include directory
If not set, directory directly above is the include directory.
For example, if library is added in src/name_space/CMakeLists.txt, headers will be installed to
``include/name_space``, but if ``INCLUDE_CURRENT`` is set than headers will be installed to ``include``.

``<NameSpace>`` is the name space for the target alias. Outside users will use library as ``<NameSpace>::<target>``.
This should match the value set in :cmake:command:`LibraryManager_Export`.
Defaults to value ``${NameSpace}`` if variable is set, otherwise no namespacing is used.

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
    cmake_parse_arguments("ARG" "INCLUDE_CURRENT" "NAMESPACE" "" ${ARGN})
    if (DEFINED ARG_NAMESPACE)
        set(NameSpace "${ARG_NAMESPACE}")
    else ()
        if (DEFINED NameSpace)
            get_filename_component(dir ${CMAKE_CURRENT_SOURCE_DIR} NAME)
            if (NOT dir STREQUAL NameSpace)
                message(FATAL_ERROR "library has to be added in ${NameSpace} directory")
            endif ()
        endif ()
    endif ()

    add_library(${target})
    if (DEFINED NameSpace)
        if (NOT NAMESPACE STREQUAL "")
            add_library(${NameSpace}::${target} ALIAS ${target})
        endif ()
    endif ()
    set_target_properties(${target} PROPERTIES __LibraryManager_NameSpace "${NameSpace}")

    message("ARG_UNPARSED_ARGUMENTS=${ARG_UNPARSED_ARGUMENTS}")
    if (DEFINED ARG_UNPARSED_ARGUMENTS)
        LibraryManager_Append(${target} ${ARG_UNPARSED_ARGUMENTS})
    endif ()
    target_include_directories(${target} PUBLIC $<INSTALL_INTERFACE:include>)
    if (ARG_INCLUDE_CURRENT)
        message("ARG_INCLUDE_CURRENT=${ARG_INCLUDE_CURRENT}")
        target_include_directories(${target} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>)
        set_target_properties(${target} PROPERTIES __LibraryManager_IncludeBaseDir "")
    else ()
        get_filename_component(dir ${CMAKE_CURRENT_SOURCE_DIR} DIRECTORY)
        target_include_directories(${target} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/..>)
        set_target_properties(${target} PROPERTIES __LibraryManager_IncludeBaseDir "${dir}")
    endif ()
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
    message("fort=${fort}")
    if (fort)
        # Are this sensible defaults?
        set_target_properties(${target} PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran)
        target_include_directories(${target}
                PUBLIC $<INSTALL_INTERFACE:include/fortran>
                $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fortran>)
    endif ()
endfunction()

function(__LibraryManager_fortranInSources out)
    foreach (src IN LISTS ARGN)
        get_filename_component(ext ${src} EXT)
        message("src=${src}")
        message("ext=${ext}")
        #todo use regex to match all possible fortran expressions
        if (ext STREQUAL ".F90")
            message("fort")
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
                   [PUBLIC_HEADER <pubHead>]
                   [<extraArgs> ...])

Installs <target> with public headers mirroing source tree structure.
Also, creates an alias library ``molpro::<target>``.

``EXPORT`` add library to specified export set.

``RUNTIME`` argument passed to install as ``RUNTIME DESTINATION <libDir>``

``LIBRARY`` argument passed to install as ``LIBRARY DESTINATION <libDir>``

``ARCHIVE`` argument passed to install as ``ARCHIVE DESTINATION <libDir>``

``PUBLIC_HEADER`` destination for public headers. The source tree structure is reproduced, matching
``<publicHead>`` and include base directory inferred in :cmake:command:`LibraryManager_Add`.
 Default value: ``include/<baseDir>``

``<extraArgs>`` are forwarded to install. Note that argument forwarding is quite limited in CMake.

.. note:: Public headers are obtained from ``<target>`` property. If they are not absolute paths, they are assumed
to be relative to source directory where ``add_library()`` was called. See cmake:command:`LibraryManager_Append`.

#]=============================================================================]
function(LibraryManager_Install target)
    set(oneValueArgs EXPORT RUNTIME LIBRARY ARCHIVE PUBLIC_HEADER)
    cmake_parse_arguments("ARG" "" "${oneValueArgs}" "" ${ARGN})
    if (NOT ARG_EXPORT)
        set(ARG_EXPORT ${target}-export)
    endif ()
    set(EXPORT "EXPORT;${ARG_EXPORT}")
    foreach (v RUNTIME LIBRARY ARCHIVE)
        set(${v} "")
        if (ARG_${v})
            set(${v} "${v};DESTINATION;${ARG_${v}}")
        endif ()
    endforeach ()
    if (NOT ARG_PUBLIC_HEADER)
        get_property(defined TARGET ${target} PROPERTY __LibraryManager_IncludeBaseDir DEFINED)
        set(dir "")
        if (defined)
            get_property(dir TARGET ${target} PROPERTY __LibraryManager_IncludeBaseDir)
        endif ()
        set(ARG_PUBLIC_HEADER include/${dir})
    endif ()

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
    foreach (src IN LISTS pubHead)
        if (IS_ABSOLUTE "${src}")
            file(RELATIVE_PATH path "${srcDir}" "${src}")
            get_filename_component(path "${path}" DIRECTORY)
        else ()
            set(src ${srcDir}/${src})
        endif ()
        install(FILES ${src} DESTINATION ${ARG_PUBLIC_HEADER}/${path})
    endforeach ()

    # install fortran module directory
    get_property(path TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY)
    if (NOT "${path}" STREQUAL "")
        get_property(path TARGET ${target} PROPERTY Fortran_MODULE_DIRECTORY)
        message("path .mod=${path}")
        install(DIRECTORY ${path} DESTINATION include OPTIONAL)
    endif ()

    # pkgconfig support TODO

    # -config configuration script support TODO
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
    set(oneValueArgs EXPORT FILE NAMESPACE DESTIONATION)
    cmake_parse_arguments("ARG" "" "${oneValueArgs}" "" ${ARGN})

    if (NOT ARG_EXPORT)
        set(ARG_EXPORT ${project}-export)
    endif ()
    if (NOT ARG_FILE)
        set(ARG_FILE ${project}Config.cmake)
    endif ()
    if (NOT ARG_NAMESPACE)
        if (NameSpace)
            set(ARG_NAMESPACE ${NameSpace}::)
        else ()
            set(ARG_NAMESPACE "")
        endif ()
    endif ()
    if (NOT ARG_DESINATION)
        set(ARG_DESINATION lib/cmake/${ARG_NAMESPACE}/${project})
    endif ()

    install(
            EXPORT ${ARG_EXPORT}
            FILE ${ARG_FILE}
            NAMESPACE "${ARG_NAMESPACE}"
            DESTINATION ${ARG_DESINATION}
    )
endfunction()
