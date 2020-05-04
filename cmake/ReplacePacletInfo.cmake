
file(READ ${COPIED_PACLETINFO} filedata)

if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"" "Version -> \"999\"(* local build *)" filedata ${filedata})

endif()

file(WRITE ${COPIED_PACLETINFO} "${filedata}")
