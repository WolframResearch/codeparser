
execute_process(
  COMMAND
    ${WOLFRAMKERNEL} -script ${SCRIPT} -srcDir ${SRCDIR} -buildDir ${BUILDDIR} -paclet ${PACLET}
  TIMEOUT
    #
    # Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes
    # So double that and cross fingers.
    #
    # Related bugs: 349779
    # Related issues: RE-514227
    #
    3600
  RESULT_VARIABLE
    SCRIPT_RESULT
)

if(NOT ${SCRIPT_RESULT} EQUAL "0")
  message(WARNING "Bad exit code from script: ${SCRIPT_RESULT}; Continuing")
endif()
