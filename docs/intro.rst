Welcome to LibraryManager!
=============================

.. contents::

Introduction
^^^^^^^^^^^^
#TODO more
:cmake:module:`LibraryManager` is a CMake module that facilitates management of
constructing software ecosystems, by which we mean
a collection of projects each providing a single solution within a particular field
favoring reliance on other solutions within the ecosystem over external dependencies.
Formation of an ecosystem is an alternative way to structure a single mega-program
by splitting it into individual repositories
and has some key advantages. Firstly, it forces a modular build and
encourages development of user friendly interface with better testing.
More importantly, it encourages more open and collaborative environment.

Projects in an ecosystem should be viewed as part of a single body,
which necessitates that they are developed side by side.
When working on a single project the source code of dependencies has
to be readily available and modifiable without risking loss of work.
The possibility of duplicate dependencies with different and some times
conflicting versions also has to be managed.

:cmake:module:`LibraryManager`  does the following. #TODO more

Authors
^^^^^^^
Marat Sibaev and Peter J. Knowles.

.. _FetchContent: https://cmake.org/cmake/help/latest/module/FetchContent.html
