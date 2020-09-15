
file(READ ${COPIED_PACLETINFO} filedata)

string(TIMESTAMP DATESTRING_COMMENT "(* Paclet built: %a %d %b %Y %H:%M:%S *)\n")

string(PREPEND filedata ${DATESTRING_COMMENT})

if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"" "Version -> \"999\"(* local build *)" filedata ${filedata})

endif()

file(WRITE ${COPIED_PACLETINFO} "${filedata}")
