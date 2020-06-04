set(NameSpace "molpro")
LibraryManager_Add(${PROJECT_NAME} NAMESPACE ${NameSpace})

add_subdirectory(${NameSpace})

LibraryManager_Install(${PROJECT_NAME} PKG_CONFIG)
LibraryManager_Export(${PROJECT_NAME} NAMESPACE ${NameSpace})
