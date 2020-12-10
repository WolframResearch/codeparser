
if(NOT EXISTS ${WOLFRAMKERNEL})
message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
endif()

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -paclet ${PACLET}
  TIMEOUT
    ${BUG349779_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
  message(FATAL_ERROR "Bad exit code from script: ${SCRIPT_RESULT}")
endif()
