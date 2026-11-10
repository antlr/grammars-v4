cmake_minimum_required(VERSION 3.14)
project(TrashCppRuntime LANGUAGES CXX)

if(NOT DEFINED TRASH_CPP_TEMPLATE_DIR)
  message(FATAL_ERROR "TRASH_CPP_TEMPLATE_DIR must identify the C++ templates")
endif()

include("${TRASH_CPP_TEMPLATE_DIR}/cmake/st.ExternalAntlr4Cpp.cmake")
add_custom_target(build_runtime ALL DEPENDS antlr4_static)
