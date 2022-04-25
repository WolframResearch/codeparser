
macro(CheckPacletInfo)

  if(NOT EXISTS ${WOLFRAMKERNEL})
  message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
  endif()

  if(LOCAL_BUILD)
    message(STATUS "Paclet Version ignored in local build")
    set(LOCAL_BUILD_VERSION 999.9)
  else()
    #
    # if not local build, then get Version from PacletInfo.m
    #
    execute_process(
      COMMAND
        ${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -nostartuppaclets -runfirst Pause[${KERNEL_PAUSE}]\;Print[OutputForm[Version\ /.\ List\ @@\ Get["${PACLETINFO_IN_SOURCE}"]]]\;Exit[]
      OUTPUT_VARIABLE
        PACLET_VERSION
      OUTPUT_STRIP_TRAILING_WHITESPACE
      WORKING_DIRECTORY
        ${PROJECT_SOURCE_DIR}
      TIMEOUT
        ${KERNEL_TIMEOUT}
      RESULT_VARIABLE
        PACLETINFO_RESULT
    )

    if(NOT ${PACLETINFO_RESULT} EQUAL "0")
      message(FATAL_ERROR "Bad exit code from PacletInfo script: ${PACLETINFO_RESULT}")
    endif()

    message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")
    
  endif(LOCAL_BUILD)

endmacro(CheckPacletInfo)
