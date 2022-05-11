
macro(CheckPacletInfo)

  if(NOT EXISTS ${WOLFRAMKERNEL})
  message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
  endif()

  if(LOCAL_BUILD)
    message(STATUS "Paclet Version ignored in local build")
    set(LOCAL_BUILD_VERSION 999.9)
  else()
    #
    # if not local build, then get Version from PacletInfo.wl
    #
    execute_process(
      COMMAND
        ${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -nostartuppaclets -runfirst Pause[${KERNEL_PAUSE}]\;Print[OutputForm[Row[{Version,\ ";",\ WolframVersion}\ /.\ List\ @@\ Get["${PACLETINFO_IN_SOURCE}"]]]]\;Exit[]
      OUTPUT_VARIABLE
        PACLET_VERSIONS_LIST
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

    list(GET PACLET_VERSIONS_LIST 0 PACLET_VERSION)
    list(GET PACLET_VERSIONS_LIST 1 PACLET_WOLFRAMVERSION)
    message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")
    message(STATUS "PACLET_WOLFRAMVERSION: ${PACLET_WOLFRAMVERSION}")
    
  endif(LOCAL_BUILD)

endmacro(CheckPacletInfo)
