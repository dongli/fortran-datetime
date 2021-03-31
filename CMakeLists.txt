cmake_minimum_required(VERSION 3.0)

set(CMAKE_MACOSX_RPATH ON)

project(FortranDatetime Fortran)

set(version 0.0.2)
 
if (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -ffree-line-length-none")
endif ()
set(CMAKE_Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}")

set(sources
  src/timedelta_mod.F90
  src/datetime_mod.F90
  src/datetime.F90
)

add_library(fortran_datetime STATIC ${sources})
set_property(TARGET fortran_datetime PROPERTY VERSION ${version})
set_property(TARGET fortran_datetime PROPERTY SOVERSION 1)

get_directory_property(parent_dir PARENT_DIRECTORY)
if (EXISTS ${PROJECT_SOURCE_DIR}/lib/unit-test/CMakeLists.txt AND NOT parent_dir)
  add_subdirectory("lib/unit-test")
  include_directories(${UNIT_TEST_INCLUDE_DIR})

  add_executable(timedelta_test "${PROJECT_SOURCE_DIR}/src/timedelta_test.F90")
  target_link_libraries(timedelta_test fortran_unit_test fortran_datetime)

  add_executable(datetime_test "${PROJECT_SOURCE_DIR}/src/datetime_test.F90")
  target_link_libraries(datetime_test fortran_unit_test fortran_datetime)
endif ()

install(TARGETS fortran_datetime EXPORT FortranDatetimeTargets
  ARCHIVE DESTINATION lib
  LIBRARY DESTINATION lib
)

install(FILES
  "${CMAKE_BINARY_DIR}/datetime_mod.mod"
  "${CMAKE_BINARY_DIR}/timedelta_mod.mod"
  DESTINATION include/fortran_datetime
)

include(CMakePackageConfigHelpers)
write_basic_package_version_file (
  "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfigVersion.cmake"
  VERSION ${version}
  COMPATIBILITY AnyNewerVersion
)

export(EXPORT FortranDatetimeTargets
  FILE "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeTargets.cmake"
)
configure_file(cmake/FortranDatetimeConfig.cmake
  "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfig.cmake"
)

set(ConfigPackageLocation lib/cmake/FortranDatetime)
install(EXPORT FortranDatetimeTargets
  FILE FortranDatetimeTargets.cmake
  DESTINATION ${ConfigPackageLocation}
)
install(
  FILES
    "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfig.cmake"
    "${CMAKE_CURRENT_BINARY_DIR}/FortranDatetime/FortranDatetimeConfigVersion.cmake"
  DESTINATION
    ${ConfigPackageLocation}
)
