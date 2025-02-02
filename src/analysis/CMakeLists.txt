# CMakeLists.txt for building the LLVM safe assembly analysis pass.

cmake_minimum_required(VERSION 3.10)
project(SafeAssemblyAnalysisPass)

find_package(LLVM REQUIRED CONFIG)
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

add_definitions(${LLVM_DEFINITIONS})
include_directories(${LLVM_INCLUDE_DIRS})

add_library(SafeAssemblyAnalysis STATIC SafeAssemblyAnalysis.cpp)
llvm_map_components_to_libnames(llvm_libs support core irreader)
target_link_libraries(SafeAssemblyAnalysis ${llvm_libs})
