cmake_minimum_required (VERSION 2.8)

set (CMAKE_MACOSX_RPATH ON)

project (FortranDatetime Fortran)

set (version 0.0.1)
 
set (CMAKE_Fortran_FLAGS "-ffree-line-length-none")

find_package (FortranUnitTest)

set (sources
  "${PROJECT_SOURCE_DIR}/src/timedelta_mod.F90"
  "${PROJECT_SOURCE_DIR}/src/datetime_mod.F90"
)

add_library (fortran_datetime SHARED ${sources})
set_property (TARGET fortran_datetime PROPERTY VERSION ${version})
set_property (TARGET fortran_datetime PROPERTY SOVERSION 1)

add_executable (timedelta_test "${PROJECT_SOURCE_DIR}/src/timedelta_test.F90")
target_link_libraries (timedelta_test fortran_unit_test fortran_datetime)

add_executable (datetime_test "${PROJECT_SOURCE_DIR}/src/datetime_test.F90")
target_link_libraries (datetime_test fortran_unit_test fortran_datetime)

install (TARGETS fortran_datetime EXPORT FortranDatetimeTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
  PUBLIC_HEADER DESTINATION include
)

include (CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export (EXPORT FortranDatetimeTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeTargets.cmake"
  NAMESPACE FortranDatetime::
)
configure_file (cmake/FortranDatetimeConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfig.cmake"
  COPYONLY
)

set (ConfigPackageLocation lib/cmake/FortranDatetime)
install (EXPORT FortranDatetimeTargets
  FILE FortranDatetimeTargets.cmake
  NAMESPACE FortranDatetime::
  DESTINATION ${ConfigPackageLocation}
)
install (
  FILES
    cmake/FortranDatetimeConfig.cmake
    "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
