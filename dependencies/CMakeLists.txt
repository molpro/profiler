include(FetchContent)
FetchContent_Declare(
        dependency_manager
        GIT_REPOSITORY https://github.com/molpro/dependency-manager.git
        GIT_TAG 0.3.0
)
FetchContent_MakeAvailable(dependency_manager)
set(CMAKE_MODULE_PATH "${CMAKE_MODULE_PATH}" PARENT_SCOPE)

DependencyManager_Declare(utilities https://github.com/molpro/utilities.git)
