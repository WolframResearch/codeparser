
execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -paclet ${PACLET}
  TIMEOUT
    300
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
  message(WARNING "Bad exit code from script: ${SCRIPT_RESULT}; Continuing")
endif()
