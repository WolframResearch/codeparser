
file(READ ${COPIED_PACLETINFO} filedata)

string(TIMESTAMP DATESTRING "%a %d %b %Y %H:%M:%S")

string(REGEX REPLACE "BuildDate -> \"[a-zA-Z0-9 :]*\"" "BuildDate -> \"${DATESTRING}\"" filedata ${filedata})

string(REGEX REPLACE "BuildNumber -> [0-9]+" "BuildNumber -> ${BUILDNUMBER}" filedata ${filedata})

if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"," "Version -> \"${LOCAL_BUILD_VERSION}\"(* local build *)," filedata ${filedata})

#
# Handle "Documentation" in list of extensions
# Fails if "Documentation" extension is first in list of extensions
#
string(REGEX REPLACE ",\n([ \t]*)\\{\"Documentation\"[a-zA-Z0-9 ->\"/]*\\}(,|)" "\\2\n\\1(* no \"Documentation\" in local build *)" filedata ${filedata})

endif()

file(WRITE ${COPIED_PACLETINFO} "${filedata}")
