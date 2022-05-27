
file(READ ${COPIED_PACLETINFO} filedata)

string(TIMESTAMP DATESTRING "%a %d %b %Y %H:%M:%S")

string(REGEX REPLACE "BuildDate -> \"[a-zA-Z0-9 :]*\"" "BuildDate -> \"${DATESTRING}\"" filedata ${filedata})

string(REGEX REPLACE "BuildNumber -> [0-9]+" "BuildNumber -> ${BUILDNUMBER}" filedata ${filedata})

string(REGEX REPLACE "BuildWolframVersionNumber -> [0-9]+" "BuildWolframVersionNumber -> ${VERSION_NUMBER}" filedata ${filedata})

string(REGEX REPLACE "BuildWolframLibraryVersion -> [0-9]+" "BuildWolframLibraryVersion -> ${WOLFRAMLIBRARY_VERSION}" filedata ${filedata})

if(USE_EXPR_LIB)
set(USE_EXPR_LIB_VAL "True")
else()
set(USE_EXPR_LIB_VAL "False")
endif()

string(REGEX REPLACE "UseExprLib -> [a-zA-Z]+" "UseExprLib -> ${USE_EXPR_LIB_VAL}" filedata ${filedata})

if(USE_MATHLINK)
set(USE_MATHLINK_VAL "True")
else()
set(USE_MATHLINK_VAL "False")
endif()

string(REGEX REPLACE "UseMathLink -> [a-zA-Z]+" "UseMathLink -> ${USE_MATHLINK_VAL}" filedata ${filedata})

if(LOCAL_BUILD)

string(REGEX REPLACE "Version -> \"[0-9\\.]+\"," "Version -> \"${LOCAL_BUILD_VERSION}\"(* local build *)," filedata ${filedata})

#
# Handle "Documentation" in list of extensions
# Fails if "Documentation" extension is first in list of extensions
#
string(REGEX REPLACE ",\n([ \t]*)\\{\"Documentation\"[a-zA-Z0-9 ->\"/]*\\}(,|)" "\\2\n\\1(* no \"Documentation\" in local build *)" filedata ${filedata})

endif()

file(WRITE ${COPIED_PACLETINFO} "${filedata}")
