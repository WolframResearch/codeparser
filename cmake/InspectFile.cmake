
if(NOT CODEPARSER_EXE)
return()
endif()

if(NOT EXISTS ${CODEPARSER_EXE})
return()
endif()

execute_process(
  COMMAND
    ${CODEPARSER_EXE} -check -file ${SRC}
  RESULT_VARIABLE
    CODEPARSER_RESULT
)

if(${CODEPARSER_RESULT} EQUAL "0")
return()
endif()

#
# We know there was some problem, so now use CodeInspector to report the problem
#

if(NOT WOLFRAMKERNEL)
return()
endif()

if(NOT EXISTS ${WOLFRAMKERNEL})
return()
endif()

set(CODE "\
If[FailureQ[FindFile[\"CodeInspector`\"]], Exit[0]]\;\
Needs[\"CodeInspector`\"]\;\
Print[CodeInspector`CodeInspectSummarize[File[\"${SRC}\"]] //OutputForm]\;\
Exit[1]\
")

execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -noinit -noprompt -nopaclet -run ${CODE}
  TIMEOUT
    ${BUG349779_TIMEOUT}
)

message(FATAL_ERROR "File had fatal errors: ${SRC}")
