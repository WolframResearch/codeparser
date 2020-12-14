
file(READ ${COPIED_PACLETINFO} filedata)

string(TIMESTAMP DATESTRING_COMMENT "(* Paclet built: %a %d %b %Y %H:%M:%S *)\n")

string(PREPEND filedata ${DATESTRING_COMMENT})

if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"" "Version -> \"${LOCAL_BUILD_VERSION}\"(* local build *)" filedata ${filedata})

# try replacing {"Documentation"} extension that is not first in list
string(REGEX REPLACE ",\n([ \t]*)(\\{\"Documentation\"[a-zA-Z0-9 ->\"/]*\\})" "\n\\1(* no \"Documentation\" in local build \\2 *)" filedata ${filedata})
# now try replacing {"Documentation"} extension that IS first in list
string(REGEX REPLACE "(\\{\"Documentation\"[a-zA-Z0-9 ->\"/]*\\})" "(* no \"Documentation\" in local build \\1 *)" filedata ${filedata})

endif()

file(WRITE ${COPIED_PACLETINFO} "${filedata}")
