
macro(CheckMacOSXVersionMin)

  if(NOT EXISTS ${WOLFRAMKERNEL})
  message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
  endif()
  
  execute_process(
    COMMAND
      ${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -nostartuppaclets -run Pause[${KERNEL_PAUSE}]\;Needs["CCompilerDriver`"]\;Print[OutputForm[StringReplace[CCompilerDriver`CCompilerDriverBase`MacOSXVersionMinFlag[],\ "-mmacosx-version-min="\ ->\ ""]]]\;Exit[]
    OUTPUT_VARIABLE
      MACOSX_VERSION_MIN
    OUTPUT_STRIP_TRAILING_WHITESPACE
    WORKING_DIRECTORY
      ${PROJECT_SOURCE_DIR}
    TIMEOUT
      ${KERNEL_TIMEOUT}
    RESULT_VARIABLE
      MACOSX_VERSION_MIN_RESULT
  )

  if(NOT ${MACOSX_VERSION_MIN_RESULT} EQUAL "0")
    message(FATAL_ERROR "Bad exit code from MacOSXVersionMin script: ${MACOSX_VERSION_MIN_RESULT}")
  endif()

endmacro(CheckMacOSXVersionMin)
