# SPDX-FileCopyrightText: Atlas Engineer LLC
# SPDX-License-Identifier: BSD-3-Clause
#
# Build the C bridge against a CEF binary distribution:
#
#   cmake -S ~/Source/Lisp/cl-cef -B ~/Source/Lisp/cl-cef/build -DCEF_ROOT=/path/to/cef_binary
#   cmake --build ~/Source/Lisp/cl-cef/build
#
# CEF_ROOT is the directory that contains include/, Release/, and cmake/.
# CEF 124 or newer is required. The build produces libcl-cef.so and
# cl-cef-helper next to the CEF resources the helper needs at runtime.

cmake_minimum_required(VERSION 3.19)
project(cl-cef LANGUAGES C CXX)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "CEF links the matching configuration" FORCE)
endif()

if(NOT CEF_ROOT)
  message(FATAL_ERROR "Set CEF_ROOT to a CEF binary distribution (124 or newer).")
endif()

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} "${CEF_ROOT}/cmake")
find_package(CEF REQUIRED)

add_subdirectory(${CEF_LIBCEF_DLL_WRAPPER_PATH} libcef_dll_wrapper)

if(NOT TARGET libcef_lib)
  add_logical_target("libcef_lib" "${CEF_LIB_DEBUG}" "${CEF_LIB_RELEASE}")
endif()

set(CL_CEF_SOURCES
  source/cxx/cl-cef.cc
  source/cxx/cl-cef.h
  source/cxx/schemes.hh)

add_library(cl-cef SHARED ${CL_CEF_SOURCES})
target_compile_features(cl-cef PRIVATE cxx_std_20)
target_include_directories(cl-cef PRIVATE "${CEF_ROOT}" "${CMAKE_CURRENT_SOURCE_DIR}/source/cxx")
add_dependencies(cl-cef libcef_dll_wrapper)
target_link_libraries(cl-cef PRIVATE libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
set_target_properties(cl-cef PROPERTIES
  BUILD_RPATH "$ORIGIN"
  INSTALL_RPATH "$ORIGIN"
  OUTPUT_NAME "cl-cef")

add_executable(cl-cef-helper source/cxx/helper.cc source/cxx/schemes.hh)
target_compile_features(cl-cef-helper PRIVATE cxx_std_20)
target_include_directories(cl-cef-helper PRIVATE "${CEF_ROOT}" "${CMAKE_CURRENT_SOURCE_DIR}/source/cxx")
add_dependencies(cl-cef-helper libcef_dll_wrapper)
target_link_libraries(cl-cef-helper PRIVATE libcef_lib libcef_dll_wrapper ${CEF_STANDARD_LIBS})
set_target_properties(cl-cef-helper PROPERTIES
  BUILD_RPATH "$ORIGIN"
  INSTALL_RPATH "$ORIGIN")

if(OS_LINUX OR OS_MAC)
  COPY_FILES("cl-cef" "${CEF_BINARY_FILES}" "${CEF_BINARY_DIR}" "$<TARGET_FILE_DIR:cl-cef>")
  COPY_FILES("cl-cef" "${CEF_RESOURCE_FILES}" "${CEF_RESOURCE_DIR}" "$<TARGET_FILE_DIR:cl-cef>")
  COPY_FILES("cl-cef-helper" "${CEF_BINARY_FILES}" "${CEF_BINARY_DIR}" "$<TARGET_FILE_DIR:cl-cef-helper>")
  COPY_FILES("cl-cef-helper" "${CEF_RESOURCE_FILES}" "${CEF_RESOURCE_DIR}" "$<TARGET_FILE_DIR:cl-cef-helper>")
endif()
