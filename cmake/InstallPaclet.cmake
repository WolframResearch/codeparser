
if(NOT EXISTS ${WOLFRAMKERNEL})
message(FATAL_ERROR "WOLFRAMKERNEL does not exist. WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
endif()

set(CODE "\
Check[
res = PacletInstall[\"${PACLET_OUTPUT}\", ForceVersionInstall -> True]\;
,
Print[OutputForm[Row[{\"$VersionNumber: \", NumberForm[$VersionNumber, {2, 1}]}]]]\;
Print[OutputForm[Row[{\"Paclet WolframVersion: \", \"${PACLET_WOLFRAMVERSION}\"}]]]\;
Print[OutputForm[Row[{\"To prevent this PacletInstall::compat message, update PacletInfo.wl.in with WolframVersion -> \\\"\", NumberForm[$VersionNumber, {2, 1}] ,\"\\\" and build and install again.\"}]]];
res
,
{PacletInstall::compat}
]\;
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
    ${KERNEL_TIMEOUT}
  RESULT_VARIABLE
    INSTALL_RESULT
)

if(NOT ${INSTALL_RESULT} EQUAL "0")
  message(FATAL_ERROR "Bad exit code from install: ${INSTALL_RESULT}")
endif()
