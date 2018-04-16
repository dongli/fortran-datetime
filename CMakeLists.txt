cmake_minimum_required (VERSION 2.8)

set (CMAKE_MACOSX_RPATH ON)

project (FortranDatetime Fortran)

set (version 0.0.2)
 
set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")
set (CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/.mods")

set (sources
  "${PROJECT_SOURCE_DIR}/src/timedelta_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/datetime_mod.F90"
)

add_library (fortran_datetime SHARED ${sources})
set_property (TARGET fortran_datetime PROPERTY VERSION ${version})
set_property (TARGET fortran_datetime PROPERTY SOVERSION 1)

if (NOT CMAKE_BUILD_TYPE OR NOT CMAKE_BUILD_TYPE STREQUAL "Release")
  find_package (FortranUnitTest)

  add_executable (timedelta_test "${PROJECT_SOURCE_DIR}/src/timedelta_test.F90")
  target_link_libraries (timedelta_test fortran_unit_test fortran_datetime)

  add_executable (datetime_test "${PROJECT_SOURCE_DIR}/src/datetime_test.F90")
  target_link_libraries (datetime_test fortran_unit_test fortran_datetime)
endif ()

install (TARGETS fortran_datetime EXPORT FortranDatetimeTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

install (DIRECTORY "${CMAKE_BINARY_DIR}/fortran_datetime" DESTINATION include)

include (CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export (EXPORT FortranDatetimeTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeTargets.cmake"
)
configure_file (cmake/FortranDatetimeConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfig.cmake"
)

set (ConfigPackageLocation lib/cmake/FortranDatetime)
install (EXPORT FortranDatetimeTargets
  FILE FortranDatetimeTargets.cmake
  DESTINATION ${ConfigPackageLocation}
)
install (
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
