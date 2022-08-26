
file(READ ${PACLETINFO_IN_SOURCE} filedata)

string(TIMESTAMP DATESTRING "%a %d %b %Y %H:%M:%S")

string(REGEX REPLACE "BuildDate -> \"[a-zA-Z0-9 :]*\"" "BuildDate -> \"${DATESTRING}\"" filedata ${filedata})

string(REGEX REPLACE "BuildNumber -> [0-9]+" "BuildNumber -> ${BUILDNUMBER}" filedata ${filedata})

string(REGEX REPLACE "BuildWolframVersionNumber -> [0-9]+" "BuildWolframVersionNumber -> ${VERSION_NUMBER}" filedata ${filedata})

string(REGEX REPLACE "BuildWolframLibraryVersion -> [0-9]+" "BuildWolframLibraryVersion -> ${WOLFRAMLIBRARY_VERSION}" filedata ${filedata})

string(REGEX REPLACE "Transport -> \"[a-zA-Z]*\"" "Transport -> \"${TRANSPORT}\"" filedata ${filedata})

if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"," "Version -> \"${LOCAL_BUILD_VERSION}\"(* local build *)," filedata ${filedata})

#
# Handle "Documentation" in list of extensions
# Fails if "Documentation" extension is first in list of extensions
#
string(REGEX REPLACE ",\n([ \t]*)\\{\"Documentation\"[a-zA-Z0-9 ->\"/]*\\}(,|)" "\\2\n\\1(* no \"Documentation\" in local build *)" filedata ${filedata})

endif()

file(WRITE ${REPLACED_PACLETINFO} "${filedata}")
