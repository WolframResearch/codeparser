
if(LOCAL_BUILD)

file(READ ${COPIED_PACLETINFO} filedata)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"" "Version -> \"999\"(* local build *)" filedata ${filedata})

file(WRITE ${COPIED_PACLETINFO} "${filedata}")

endif()
