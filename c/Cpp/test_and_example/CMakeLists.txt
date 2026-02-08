cmake_minimum_required(VERSION 3.14)
project(c-grammar-cpp-test LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 23)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# ──────────────────────────────────────────────────────────
#  Paths
# ──────────────────────────────────────────────────────────
set(GRAMMAR_DIR   ${CMAKE_CURRENT_SOURCE_DIR}/..)        # grammar/Cpp/
set(GRAMMAR_ROOT  ${CMAKE_CURRENT_SOURCE_DIR}/../..)     # grammar/
set(GENERATED_DIR ${CMAKE_CURRENT_BINARY_DIR}/generated)
file(MAKE_DIRECTORY ${GENERATED_DIR})

# ──────────────────────────────────────────────────────────
#  Find ANTLR4
# ──────────────────────────────────────────────────────────
find_path(ANTLR4_INCLUDE_DIR
  NAMES antlr4-runtime.h
  PATHS /antlr4-runtime /usr/local/include/antlr4-runtime /usr/include/antlr4-runtime
)
if(NOT ANTLR4_INCLUDE_DIR)
  message(FATAL_ERROR "ANTLR4 runtime headers not found")
endif()

find_library(ANTLR4_RUNTIME_LIB
  NAMES antlr4-runtime
  PATHS /usr/local/lib
)
if(NOT ANTLR4_RUNTIME_LIB)
  message(FATAL_ERROR "ANTLR4 runtime library not found")
endif()

find_program(ANTLR_EXECUTABLE antlr4 HINTS /usr/local/bin)
if(NOT ANTLR_EXECUTABLE)
  message(FATAL_ERROR "antlr4 executable not found")
endif()

find_program(PYTHON3 python3)
if(NOT PYTHON3)
  message(FATAL_ERROR "python3 not found")
endif()

# ──────────────────────────────────────────────────────────
#  Step 1: transformGrammar.py  →  生成 Cpp 版 .g4
# ──────────────────────────────────────────────────────────
add_custom_command(
  OUTPUT
    ${GENERATED_DIR}/CLexer.g4
    ${GENERATED_DIR}/CParser.g4
  COMMAND ${PYTHON3} ${GRAMMAR_DIR}/transformGrammar.py ${GENERATED_DIR}
  DEPENDS
    ${GRAMMAR_DIR}/transformGrammar.py
    ${GRAMMAR_ROOT}/CLexer.g4
    ${GRAMMAR_ROOT}/CParser.g4
  COMMENT "Transforming grammar for Cpp target"
)

# ──────────────────────────────────────────────────────────
#  Step 2: ANTLR4  →  生成 CLexer / CParser C++ 源码
# ──────────────────────────────────────────────────────────
# Lexer (must be generated first — CParser depends on CLexer token vocab)
add_custom_command(
  OUTPUT
    ${GENERATED_DIR}/CLexer.cpp
    ${GENERATED_DIR}/CLexer.h
    ${GENERATED_DIR}/CLexer.interp
    ${GENERATED_DIR}/CLexer.tokens
  COMMAND ${ANTLR_EXECUTABLE}
    -Dlanguage=Cpp -no-listener -no-visitor
    -o ${GENERATED_DIR}
    -lib ${GENERATED_DIR}
    ${GENERATED_DIR}/CLexer.g4
  DEPENDS ${GENERATED_DIR}/CLexer.g4
  COMMENT "Generating CLexer (Cpp)"
)

# Parser (+ visitor)
add_custom_command(
  OUTPUT
    ${GENERATED_DIR}/CParser.cpp
    ${GENERATED_DIR}/CParser.h
    ${GENERATED_DIR}/CParserBaseVisitor.cpp
    ${GENERATED_DIR}/CParserBaseVisitor.h
    ${GENERATED_DIR}/CParserVisitor.cpp
    ${GENERATED_DIR}/CParserVisitor.h
    ${GENERATED_DIR}/CParser.interp
    ${GENERATED_DIR}/CParser.tokens
  COMMAND ${ANTLR_EXECUTABLE}
    -Dlanguage=Cpp -visitor -no-listener
    -o ${GENERATED_DIR}
    -lib ${GENERATED_DIR}
    ${GENERATED_DIR}/CParser.g4
  DEPENDS
    ${GENERATED_DIR}/CParser.g4
    ${GENERATED_DIR}/CLexer.tokens
  COMMENT "Generating CParser (Cpp)"
)

# ──────────────────────────────────────────────────────────
#  Copy antlr4-runtime.h into generated/ so @header includes resolve
# ──────────────────────────────────────────────────────────
add_custom_command(
  OUTPUT ${GENERATED_DIR}/antlr4-runtime.h
  COMMAND ${CMAKE_COMMAND} -E copy
    ${ANTLR4_INCLUDE_DIR}/antlr4-runtime.h
    ${GENERATED_DIR}/antlr4-runtime.h
  DEPENDS ${ANTLR4_INCLUDE_DIR}/antlr4-runtime.h
)

# ──────────────────────────────────────────────────────────
#  Sources
# ──────────────────────────────────────────────────────────
set(CPP_BASE_SOURCES
  ${GRAMMAR_DIR}/CLexerBase.cpp
  ${GRAMMAR_DIR}/CParserBase.cpp
  ${GRAMMAR_DIR}/SymbolTable.cpp
)

set(GENERATED_SOURCES
  ${GENERATED_DIR}/CLexer.cpp
  ${GENERATED_DIR}/CParser.cpp
  ${GENERATED_DIR}/CParserBaseVisitor.cpp
  ${GENERATED_DIR}/CParserVisitor.cpp
)

# ──────────────────────────────────────────────────────────
#  Test executable
# ──────────────────────────────────────────────────────────
add_executable(c_grammar_cpp_test
  ${CMAKE_CURRENT_SOURCE_DIR}/main.cpp
  ${CPP_BASE_SOURCES}
  ${GENERATED_SOURCES}
  ${GENERATED_DIR}/antlr4-runtime.h
)

target_include_directories(c_grammar_cpp_test PRIVATE
  ${GRAMMAR_DIR}          # CLexerBase.h, CParserBase.h, Symbol.h, ...
  ${GENERATED_DIR}        # CLexer.h, CParser.h, ...
  ${ANTLR4_INCLUDE_DIR}   # antlr4-runtime
)

target_link_libraries(c_grammar_cpp_test PRIVATE ${ANTLR4_RUNTIME_LIB})

# ──────────────────────────────────────────────────────────
#  CTest
# ──────────────────────────────────────────────────────────
enable_testing()

add_test(NAME parse_hello_c
  COMMAND c_grammar_cpp_test --nopp ${CMAKE_CURRENT_SOURCE_DIR}/hello.c
)

add_test(NAME parse_typedef_c
  COMMAND c_grammar_cpp_test --nopp ${CMAKE_CURRENT_SOURCE_DIR}/typedef_test.c
)
