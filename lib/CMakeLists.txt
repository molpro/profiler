set(NameSpace "molpro")
LibraryManager_Add(${PROJECT_NAME} NAMESPACE ${NameSpace})
target_include_directories(${PROJECT_NAME} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}>)

if (MPI)
    find_package(MPI COMPONENTS CXX)
endif ()

add_subdirectory(${NameSpace})

LibraryManager_Install(${PROJECT_NAME} PKG_CONFIG)
LibraryManager_Export(${PROJECT_NAME} NAMESPACE ${NameSpace})
