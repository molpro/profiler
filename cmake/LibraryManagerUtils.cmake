include_guard()
#[=============================================================================[.rst:
LibraryManagerUtils
-------------------

.. module:: LibraryManagerUtils

Overview
^^^^^^^^

Miscellaneous functions that can be helpful during development of the CMake build.

Functions
^^^^^^^^^

List of Functions:

- :cmake:command:`print_value`
- :cmake:command:`print_target_properties`

#]=============================================================================]

#[=============================================================================[.rst:
.. cmake:command:: print_variable

.. code-block:: cmake

    print_variable(<variable> [<mode>])

Prints a message giving the value of ``<variable>`` using ``message(<mode> ....)``.
``<mode>`` defaults to ``STATUS``.
#]=============================================================================]
function(print_variable variable)
    if (ARGN)
        set(MODE ${ARGN})
    else ()
        set(MODE STATUS)
    endif ()
    message(${MODE} "${variable} = ${${variable}}")
endfunction()

#[=============================================================================[.rst:
.. cmake:command:: print_target_properties

.. code-block:: cmake

    print_target_properties(<target> [<mode>])

Prints all of the properties of ``<target>`` using ``message(<mode> ....)``.
``<mode>`` defaults to ``STATUS``.
#]=============================================================================]
function(print_target_properties tgt)
    if (ARGN)
        set(MODE ${ARGN})
    else ()
        set(MODE STATUS)
    endif ()
    if (NOT TARGET ${tgt})
        message(${MODE} "There is no target named '${tgt}'")
        return()
    endif ()
    # Get all propreties that cmake supports
    execute_process(COMMAND cmake --help-property-list OUTPUT_VARIABLE CMAKE_PROPERTY_LIST)

    # Convert command output into a CMake list
    STRING(REGEX REPLACE ";" "\\\\;" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")
    STRING(REGEX REPLACE "\n" ";" CMAKE_PROPERTY_LIST "${CMAKE_PROPERTY_LIST}")

    # build whitelist by filtering down from CMAKE_PROPERTY_LIST in case cmake is
    # a different version, and one of our hardcoded whitelisted properties
    # doesn't exist!
    unset(CMAKE_WHITELISTED_PROPERTY_LIST)
    foreach (prop ${CMAKE_PROPERTY_LIST})
        if (prop MATCHES "^(INTERFACE|[_a-z]|IMPORTED_LIBNAME_|MAP_IMPORTED_CONFIG_)|^(COMPATIBLE_INTERFACE_(BOOL|NUMBER_MAX|NUMBER_MIN|STRING)|EXPORT_NAME|IMPORTED(_GLOBAL|_CONFIGURATIONS|_LIBNAME)?|NAME|TYPE|NO_SYSTEM_FROM_IMPORTED)$")
            list(APPEND CMAKE_WHITELISTED_PROPERTY_LIST ${prop})
        endif ()
    endforeach (prop)

    get_target_property(target_type ${tgt} TYPE)
    if (target_type STREQUAL "INTERFACE_LIBRARY")
        set(PROP_LIST ${CMAKE_WHITELISTED_PROPERTY_LIST})
    else ()
        set(PROP_LIST ${CMAKE_PROPERTY_LIST})
    endif ()

    foreach (prop ${PROP_LIST})
        if (prop MATCHES "^.*LOCATION.*$")
        else ()
            string(REPLACE "<CONFIG>" "${CMAKE_BUILD_TYPE}" prop ${prop})
#            message("Checking ${prop}")
            get_property(propval TARGET ${tgt} PROPERTY ${prop} SET)
            if (propval)
                get_target_property(propval ${tgt} ${prop})
                message(${MODE} "${tgt} ${prop} = ${propval}")
            endif ()
        endif ()
    endforeach (prop)
endfunction(print_target_properties)
