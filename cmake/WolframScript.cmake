
if(NOT EXISTS ${WOLFRAMKERNEL})
message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
endif()

if(NOT DEFINED RETRY_ON_FAILURE)
set(RETRY_ON_FAILURE OFF)
endif()

if(NOT EXISTS ${SCRIPT})
message(FATAL_ERROR "SCRIPT does not exist. SCRIPT: ${SCRIPT}")
endif()

file(READ ${SCRIPT} script)

if(script STREQUAL "")
message(FATAL_ERROR "SCRIPT is empty. SCRIPT: ${SCRIPT}")
endif()

if(RETRY_ON_FAILURE)

#
# try twice
#

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -pacletLayoutDir ${PACLET_LAYOUT_DIR} -paclet ${PACLET}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(WARNING "First try: Bad exit code from script: ${SCRIPT_RESULT}; retrying...")

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -retry -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -pacletLayoutDir ${PACLET_LAYOUT_DIR} -paclet ${PACLET}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(FATAL_ERROR "Second try: Bad exit code from script: ${SCRIPT_RESULT}; stopping")
else()
message(STATUS "Second try: Success!")
endif()

endif()

else(RETRY_ON_FAILURE)

#
# only try once
#

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -pacletLayoutDir ${PACLET_LAYOUT_DIR} -paclet ${PACLET}
  TIMEOUT
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
message(FATAL_ERROR "Bad exit code from script: ${SCRIPT_RESULT} (script was ${SCRIPT})")
endif()

endif()
