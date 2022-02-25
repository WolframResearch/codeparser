
if(NOT EXISTS ${WOLFRAMKERNEL})
message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
endif()

if(NOT DEFINED RETRY_ON_FAILURE)
set(RETRY_ON_FAILURE OFF)
endif()

if(RETRY_ON_FAILURE)

#
# try twice
#

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -paclet ${PACLET}
  TIMEOUT
    ${BUG349779_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(WARNING "First try: Bad exit code from script: ${SCRIPT_RESULT}; retrying...")

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -paclet ${PACLET}
  TIMEOUT
    ${BUG349779_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(FATAL_ERROR "Second try: Bad exit code from script: ${SCRIPT_RESULT}; stopping")
endif()

endif()

else(RETRY_ON_FAILURE)

#
# only try once
#

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

endif()
