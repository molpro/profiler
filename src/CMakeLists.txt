LibraryManager_Add(${PROJECT_NAME} NAMESPACE molpro)

add_subdirectory(molpro)

LibraryManager_Install(${PROJECT_NAME})
LibraryManager_Export(${PROJECT_NAME})
