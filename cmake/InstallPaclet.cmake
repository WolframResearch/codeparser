
if(NOT EXISTS ${WOLFRAMKERNEL})
message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
endif()

set(CODE "\
res = PacletInstall[\"${PACLET_OUTPUT}\", ForceVersionInstall -> True]\;
Print[res //OutputForm]\;
If[!PacletObjectQ[res],
  Exit[1]
]\;
Exit[0]
")

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -noinit -noprompt -run ${CODE}
  TIMEOUT
    ${BUG349779_TIMEOUT}
  RESULT_VARIABLE
    INSTALL_RESULT
)

if(NOT ${INSTALL_RESULT} EQUAL "0")
  message(FATAL_ERROR "Bad exit code from install: ${INSTALL_RESULT}")
endif()
