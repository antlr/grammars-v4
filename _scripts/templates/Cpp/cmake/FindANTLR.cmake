find_package(Java QUIET COMPONENTS Runtime)

  execute_process(
      COMMAND antlr4<if(os_win)>.exe<else><endif> -v ${ANTLR4_TAG}
      OUTPUT_VARIABLE ANTLR_COMMAND_OUTPUT
      ERROR_VARIABLE ANTLR_COMMAND_ERROR
      RESULT_VARIABLE ANTLR_COMMAND_RESULT
      OUTPUT_STRIP_TRAILING_WHITESPACE)

  if(ANTLR_COMMAND_RESULT EQUAL 0)
    string(REGEX MATCH "Version [0-9]+(\\\\.[0-9]+)*" ANTLR_VERSION ${ANTLR_COMMAND_OUTPUT})
    string(REPLACE "Version " "" ANTLR_VERSION ${ANTLR_VERSION})
  else()
    message(
        SEND_ERROR
        "Command antlr4<if(os_win)>.exe<else><endif> "
        "failed with the output '${ANTLR_COMMAND_ERROR}'")
  endif()

  macro(ANTLR_TARGET Name InputFile)
    set(ANTLR_OPTIONS LEXER PARSER LISTENER VISITOR)
    set(ANTLR_ONE_VALUE_ARGS PACKAGE OUTPUT_DIRECTORY DEPENDS_ANTLR)
    set(ANTLR_MULTI_VALUE_ARGS COMPILE_FLAGS DEPENDS)
    cmake_parse_arguments(ANTLR_TARGET
                          "${ANTLR_OPTIONS}"
                          "${ANTLR_ONE_VALUE_ARGS}"
                          "${ANTLR_MULTI_VALUE_ARGS}"
                          ${ARGN})

    set(ANTLR_${Name}_INPUT ${InputFile})

    get_filename_component(ANTLR_INPUT ${InputFile} NAME_WE)

    if(ANTLR_TARGET_OUTPUT_DIRECTORY)
      set(ANTLR_${Name}_OUTPUT_DIR ${ANTLR_TARGET_OUTPUT_DIRECTORY})
    else()
      set(ANTLR_${Name}_OUTPUT_DIR
          ${CMAKE_CURRENT_BINARY_DIR}/antlr4cpp_generated_src/${ANTLR_INPUT})
    endif()

    unset(ANTLR_${Name}_CXX_OUTPUTS)

    if((ANTLR_TARGET_LEXER AND NOT ANTLR_TARGET_PARSER) OR
       (ANTLR_TARGET_PARSER AND NOT ANTLR_TARGET_LEXER))
      list(APPEND ANTLR_${Name}_CXX_OUTPUTS
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}.cpp)
      set(ANTLR_${Name}_OUTPUTS
          ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}.interp
          ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}.tokens)
    else()
      list(APPEND ANTLR_${Name}_CXX_OUTPUTS
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Lexer.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Lexer.cpp
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Parser.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Parser.cpp)
      list(APPEND ANTLR_${Name}_OUTPUTS
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Lexer.interp
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Lexer.tokens)
    endif()

    if(ANTLR_TARGET_LISTENER)
      list(APPEND ANTLR_${Name}_CXX_OUTPUTS
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}BaseListener.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}BaseListener.cpp
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Listener.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Listener.cpp)
      list(APPEND ANTLR_TARGET_COMPILE_FLAGS -listener)
    endif()

    if(ANTLR_TARGET_VISITOR)
      list(APPEND ANTLR_${Name}_CXX_OUTPUTS
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}BaseVisitor.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}BaseVisitor.cpp
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Visitor.h
           ${ANTLR_${Name}_OUTPUT_DIR}/${ANTLR_INPUT}Visitor.cpp)
      list(APPEND ANTLR_TARGET_COMPILE_FLAGS -visitor)
    endif()

    if(ANTLR_TARGET_PACKAGE)
      list(APPEND ANTLR_TARGET_COMPILE_FLAGS -package ${ANTLR_TARGET_PACKAGE})
    endif()

    list(APPEND ANTLR_${Name}_OUTPUTS ${ANTLR_${Name}_CXX_OUTPUTS})

    if(ANTLR_TARGET_DEPENDS_ANTLR)
      if(ANTLR_${ANTLR_TARGET_DEPENDS_ANTLR}_INPUT)
        list(APPEND ANTLR_TARGET_DEPENDS
             ${ANTLR_${ANTLR_TARGET_DEPENDS_ANTLR}_INPUT})
        list(APPEND ANTLR_TARGET_DEPENDS
             ${ANTLR_${ANTLR_TARGET_DEPENDS_ANTLR}_OUTPUTS})
      else()
        message(SEND_ERROR
                "ANTLR target '${ANTLR_TARGET_DEPENDS_ANTLR}' not found")
      endif()
    endif()

    add_custom_command(
        OUTPUT ${ANTLR_${Name}_OUTPUTS}
        COMMAND antlr4<if(os_win)>.exe<else><endif>
                -v ${ANTLR4_TAG}
                ${InputFile}
                -o ${ANTLR_${Name}_OUTPUT_DIR}
                -no-listener
                -Dlanguage=Cpp
                ${ANTLR_TARGET_COMPILE_FLAGS}
        DEPENDS ${InputFile}
                ${ANTLR_TARGET_DEPENDS}
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        COMMENT "Building ${Name} with ANTLR ${ANTLR_VERSION}")
  endmacro(ANTLR_TARGET)

