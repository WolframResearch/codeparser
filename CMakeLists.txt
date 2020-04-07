
cmake_minimum_required(VERSION 3.0)

project(codeparser)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)

set(PACLET "CodeParser")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(BUILD_DOCS OFF CACHE BOOL "Build documentation")
set(BUILD_EXE OFF CACHE BOOL "Build executable")
set(BUILD_EXPR_LIB ON CACHE BOOL "Build expr library")
set(USE_MATHLINK ON CACHE BOOL "Use MathLink")
set(NISSUES OFF CACHE BOOL "NISSUES")
set(NABORT OFF CACHE BOOL "NABORT")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")

message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")
message(STATUS "BUILD_EXE: ${BUILD_EXE}")
message(STATUS "BUILD_EXPR_LIB: ${BUILD_EXPR_LIB}")
message(STATUS "USE_MATHLINK: ${USE_MATHLINK}")
message(STATUS "NISSUES: ${NISSUES}")
message(STATUS "NABORT: ${NABORT}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")

set(STATIC_WL_PACLET_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Abstract.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/CodeParser.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Boxes.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/CodeAction.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Definitions.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Error.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Folds.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Library.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Node.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Quirks.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Shims.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/ToString.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Utils.wl
)

set(WL_PACLET_EXAMPLES
	${PROJECT_SOURCE_DIR}/CodeParser/Resources/Examples/Collatz.m
)

set(GENERATED_WL_PACLET_SOURCES
	${PROJECT_BINARY_DIR}/generated/wl/LongNames.wl
)

#
# Used during the build process
#
set(WL_GENERATE_SOURCES_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/GenerateSources.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/LongNames.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/ParseletRegistration.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/Precedence.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/Symbol.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/TokenEnum.wl
)

set(WL_GENERATE_EXPRLIB_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/GenerateExpressionLibrary.wl
)

set(WL_GENERATE_CREATEPACLETARCHIVE_SOURCES
	${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
)

set(PACLETINFO_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m
)

set(WL_DATA_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Data/LongNames.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Data/Precedence.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Data/TokenEnum.wl
)

set(GENERATED_CPP_INCLUDES
	${PROJECT_BINARY_DIR}/generated/cpp/include/LongNames.h
	${PROJECT_BINARY_DIR}/generated/cpp/include/ParseletRegistration.h
	${PROJECT_BINARY_DIR}/generated/cpp/include/Precedence.h
	${PROJECT_BINARY_DIR}/generated/cpp/include/Symbol.h
	${PROJECT_BINARY_DIR}/generated/cpp/include/TokenEnum.h
)

set(GENERATED_CPP_LIB_SOURCES
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/LongNames.cpp
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/ParseletRegistration.cpp
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/Symbol.cpp
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/TokenEnum.cpp
)

set(GENERATED_SOURCES
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_LIB_SOURCES}
	${GENERATED_WL_PACLET_SOURCES}
)


set(DOCUMENTATION_NOTEBOOK_PATHS
	English/Guides/CodeParser.nb
	English/ReferencePages/Symbols/CodeConcreteParse.nb
	English/ReferencePages/Symbols/CodeParse.nb
	English/ReferencePages/Symbols/CodeTokenize.nb
	English/Tutorials/ReleaseNotes.nb
)

#
# Setup documentation notebook paths
#
foreach(NB_PATH ${DOCUMENTATION_NOTEBOOK_PATHS})
	set(SRC ${PROJECT_SOURCE_DIR}/${PACLET}/Documentation/${NB_PATH})
	set(BUILT ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Documentation/${NB_PATH})
	list(APPEND DOCUMENTATION_SOURCE ${SRC})
	list(APPEND BUILT_DOCUMENTATION ${BUILT})
endforeach()



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()


if(BUILD_EXPR_LIB)
if(${VERSION_NUMBER} LESS 1210)
set(BUILD_EXPR_LIB OFF)
message(STATUS "VERSION_NUMBER is below 1210; disabling BUILD_EXPR_LIB")
endif()
endif()



file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
)


#
# Copy WL source files
#

set(COPIED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

add_custom_command(
	OUTPUT
		${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -E copy ${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m ${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DLOCAL_BUILD=${LOCAL_BUILD} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.m
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)
list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

foreach(SRC ${STATIC_WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
endforeach()

foreach(SRC ${WL_PACLET_EXAMPLES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Examples/${BARE_SRC}
    COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Examples/${BARE_SRC}
    DEPENDS ${SRC})
	list(APPEND COPIED_WL_PACLET_EXAMPLES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/Examples/${BARE_SRC})
endforeach()

foreach(SRC ${GENERATED_WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Generated/${BARE_SRC}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Generated/${BARE_SRC}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Generated/${BARE_SRC})
endforeach()



#
# generated-srcs target
#
add_custom_target(generated-srcs
	DEPENDS
		${GENERATED_SOURCES}
)

#
# Generate source files
#
# Run the GenerateSources.wl script to generate additional required source files
#
add_custom_command(
	OUTPUT
		${GENERATED_SOURCES}
	DEPENDS
		${WL_DATA_SOURCES}
		${WL_GENERATE_SOURCES_SOURCES}
		${PROJECT_SOURCE_DIR}/${PACLET}/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/GenerateSources.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)






#
# expr lib target
#
if(BUILD_EXPR_LIB)

set(EXPR_LIB ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/expr${CMAKE_SHARED_LIBRARY_SUFFIX})

add_library(expr-lib SHARED IMPORTED)

set_property(
	TARGET
		expr-lib
	PROPERTY
	IMPORTED_LOCATION
		${EXPR_LIB}
)

add_custom_target(expr-lib-custom
	DEPENDS
		${EXPR_LIB}
)

add_custom_command(
	OUTPUT
		${EXPR_LIB}
	DEPENDS
		${WL_GENERATE_EXPRLIB_SOURCES}
		${PROJECT_SOURCE_DIR}/${PACLET}/Generate/GenerateExpressionLibrary.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -E echo "EXPR_LIB: ${EXPR_LIB}"
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/GenerateExpressionLibrary.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

add_dependencies(expr-lib expr-lib-custom)

endif(BUILD_EXPR_LIB)





#
# Build CodeParser library
#

set(WOLFRAMLIBRARY_INCLUDE_DIR ${WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

if(USE_MATHLINK)

set(MATHLINK_INCLUDE_DIR ${MATHLINK_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to mathlink.h")

message(STATUS "MATHLINK_INCLUDE_DIR: ${MATHLINK_INCLUDE_DIR}")

set(MATHLINK_LIB_DIR ${MATHLINK_LIB_DIR_DEFAULT} CACHE FILEPATH "Path to MathLink library")

message(STATUS "MATHLINK_LIB_DIR: ${MATHLINK_LIB_DIR}")

endif(USE_MATHLINK)

set(CPP_INCLUDES
	${PROJECT_SOURCE_DIR}/cpp/include/API.h
	${PROJECT_SOURCE_DIR}/cpp/include/ByteBuffer.h
	${PROJECT_SOURCE_DIR}/cpp/include/ByteDecoder.h
	${PROJECT_SOURCE_DIR}/cpp/include/ByteEncoder.h
	${PROJECT_SOURCE_DIR}/cpp/include/CharacterDecoder.h
	${PROJECT_SOURCE_DIR}/cpp/include/CodePoint.h
	${PROJECT_SOURCE_DIR}/cpp/include/Node.h
	${PROJECT_SOURCE_DIR}/cpp/include/Parselet.h
	${PROJECT_SOURCE_DIR}/cpp/include/Parser.h
	${PROJECT_SOURCE_DIR}/cpp/include/Source.h
	${PROJECT_SOURCE_DIR}/cpp/include/Token.h
	${PROJECT_SOURCE_DIR}/cpp/include/Tokenizer.h
	${PROJECT_SOURCE_DIR}/cpp/include/Utils.h
	${PROJECT_SOURCE_DIR}/cpp/include/WLCharacter.h
)

if(BUILD_EXPR_LIB)
list(APPEND CPP_INCLUDES
	${PROJECT_SOURCE_DIR}/cpp/include/ExpressionLibrary.h
)
endif()



set(CPP_LIB_SOURCES
	${PROJECT_SOURCE_DIR}/cpp/src/lib/API.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/ByteBuffer.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/ByteDecoder.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/ByteEncoder.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/CharacterDecoder.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Node.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Parselet.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Parser.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/SemiSemiParselet.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Source.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Token.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Tokenizer.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Utils.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/WLCharacter.cpp
)

if(BUILD_EXPR_LIB)
list(APPEND CPP_LIB_SOURCES
${PROJECT_SOURCE_DIR}/cpp/src/lib/ExpressionLibrary.cpp
)
endif()



add_library(codeparser-lib SHARED
	${CPP_INCLUDES}
	${CPP_LIB_SOURCES}
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_LIB_SOURCES}
)

target_include_directories(codeparser-lib
	PRIVATE ${PROJECT_SOURCE_DIR}/cpp/include
	PRIVATE ${PROJECT_BINARY_DIR}/generated/cpp/include
	PRIVATE ${MATHLINK_INCLUDE_DIR}
	PRIVATE ${WOLFRAMLIBRARY_INCLUDE_DIR}
)



if(USE_MATHLINK)

find_library(MATHLINK_LIB
	#
	# mathlink is the mathlink.framework on MacOSX
	#
	# in version 12.2: ml64i4m has become the (only) official name of the Windows import library
	# Related issues: RE-514316
	#
	NAMES mathlink ml64i4 ml32i4 ML64i4 ml64i4m
	HINTS ${MATHLINK_LIB_DIR}
)

message(STATUS "MATHLINK_LIB: ${MATHLINK_LIB}")

target_link_libraries(codeparser-lib ${MATHLINK_LIB})

endif(USE_MATHLINK)

if(BUILD_EXPR_LIB)
target_link_libraries(codeparser-lib expr-lib)
endif()

set_target_properties(codeparser-lib PROPERTIES
	OUTPUT_NAME
		CodeParser
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#library-output-artifacts
	#
	LIBRARY_OUTPUT_DIRECTORY
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	#
	LIBRARY_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#archive-output-artifacts
	#
	ARCHIVE_OUTPUT_DIRECTORY
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#runtime-output-artifacts
	#
	RUNTIME_OUTPUT_DIRECTORY
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_DEBUG
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# WL / LibraryLink style dictates that the library be named "CodeParser.dylib" and not "libCodeParser.dylib"
	#
	PREFIX
		""
)


if(NISSUES)
target_compile_definitions(codeparser-lib PUBLIC NISSUES=1)
endif()

if(NABORT)
target_compile_definitions(codeparser-lib PUBLIC NABORT=1)
endif()

if(USE_MATHLINK)
target_compile_definitions(codeparser-lib PUBLIC USE_MATHLINK=1)
endif()

target_compile_definitions(codeparser-lib PUBLIC SIZEOF_VOID_P=${CMAKE_SIZEOF_VOID_P})



#
# Setup warnings
#
if(MSVC)
	target_compile_options(codeparser-lib
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else(MSVC)
	target_compile_options(codeparser-lib
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)



if(BUILD_EXE)

add_subdirectory(cpp/src/exe)

endif(BUILD_EXE)



if(BUILD_TESTS)

add_subdirectory(cpp/test)

endif(BUILD_TESTS)



#
# docs target
#

if(BUILD_DOCS)

add_custom_target(docs
	DEPENDS
		${BUILT_DOCUMENTATION}
)

set(DOCS_APPPATH /Applications/Eclipse.app/Contents/Eclipse/configuration/org.eclipse.osgi/525/0/.cp/MathematicaSource/ CACHE PATH "Path of MathematicaSource inside Workbench installation")

message(STATUS "DOCS_APPPATH: ${DOCS_APPPATH}")

if (NOT IS_DIRECTORY ${DOCS_APPPATH})
message(FATAL_ERROR "DOCS_APPPATH (${DOCS_APPPATH}) does not exist")
endif(NOT IS_DIRECTORY ${DOCS_APPPATH})

set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

message(STATUS "DOCS_JLINKPATH: ${DOCS_JLINKPATH}")

if (NOT IS_DIRECTORY ${DOCS_JLINKPATH})
message(FATAL_ERROR "DOCS_JLINKPATH (${DOCS_JLINKPATH}) does not exist")
endif(NOT IS_DIRECTORY ${DOCS_JLINKPATH})


#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION}
	DEPENDS
		${DOCUMENTATION_SOURCE}
	COMMAND
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${PROJECT_SOURCE_DIR}/${PACLET}/Documentation/ -DoutputDir=${PROJECT_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

endif(BUILD_DOCS)




#
# paclet target
#

set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
	${COPIED_WL_PACLET_EXAMPLES}
	generated-srcs
	codeparser-lib
	${WL_GENERATE_CREATEPACLETARCHIVE_SOURCES}
)

if(BUILD_EXPR_LIB)
list(APPEND PACLET_SOURCES
	expr-lib
)
endif()

if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION}
)

endif(BUILD_DOCS)

add_custom_target(paclet
	DEPENDS
		${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/CreatePacletArchive.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET=${PACLET} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)



