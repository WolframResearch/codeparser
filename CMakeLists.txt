
cmake_minimum_required(VERSION 3.0)

project(codeparser
	LANGUAGES
		CXX
)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)

#
# Used for quickly reporting syntax errors in WL source files
#
find_program(CODEPARSER_EXE
	NAMES
		codeparser codeparser.exe
	HINTS
		${CODEPARSER_EXE_DIR}
)

set(PACLET "CodeParser")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(BUILD_EXE OFF CACHE BOOL "Build executable")
set(BUILD_EXPR_LIB OFF CACHE BOOL "Build experimental expr library")
set(USE_MATHLINK ON CACHE BOOL "Use MathLink")
set(NISSUES OFF CACHE BOOL "NISSUES")
set(NABORT OFF CACHE BOOL "NABORT")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")
# Work-around for bug 349779 is to pause ~1 second
set(BUG349779_PAUSE 1 CACHE STRING "Bug 349779 pause")
#
# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes
# So double that and cross fingers.
#
# Related bugs: 349779
# Related issues: RE-514227
#
set(BUG349779_TIMEOUT 3600 CACHE STRING "Bug 349779 timeout")

message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_EXE: ${BUILD_EXE}")
message(STATUS "BUILD_EXPR_LIB: ${BUILD_EXPR_LIB}")
message(STATUS "USE_MATHLINK: ${USE_MATHLINK}")
message(STATUS "NISSUES: ${NISSUES}")
message(STATUS "NABORT: ${NABORT}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")
message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "BUG349779_PAUSE: ${BUG349779_PAUSE}")
message(STATUS "BUG349779_TIMEOUT: ${BUG349779_TIMEOUT}")
if(CODEPARSER_EXE)
message(STATUS "CODEPARSER_EXE: ${CODEPARSER_EXE}")
else(CODEPARSER_EXE)
message(STATUS "Optional tool CODEPARSER_EXE was not found; skipping")
endif(CODEPARSER_EXE)

set(STATIC_WL_PACLET_KERNEL_SOURCES
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
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Scoping.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Shims.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/ToString.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Utils.wl
)

set(STATIC_WL_PACLET_RESOURCES_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Resources/Examples/Collatz.m
)

set(PACLETINFO_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.wl.in
)

set(GENERATED_WL_PACLET_KERNEL_SOURCES
	${PROJECT_BINARY_DIR}/generated/wl/Kernel/RowBox.wl
)

set(GENERATED_WL_PACLET_RESOURCES_SOURCES
	${PROJECT_BINARY_DIR}/generated/wl/Resources/Generated/LongNames.wl
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



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()


if(BUILD_EXPR_LIB)
if(${VERSION_NUMBER} LESS 1220)
set(BUILD_EXPR_LIB OFF)
message(STATUS "VERSION_NUMBER is below 1220; disabling BUILD_EXPR_LIB")
endif()
endif()



file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
	${PROJECT_BINARY_DIR}/generated/cpp/include/
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/
	${PROJECT_BINARY_DIR}/generated/wl/Resources/Generated/
)


#
# Copy WL source files
#

set(COPIED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.wl)

add_custom_command(
	OUTPUT
		${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -E copy ${PACLETINFO_SOURCE} ${COPIED_PACLETINFO}
	COMMAND
		${CMAKE_COMMAND} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DCOPIED_PACLETINFO=${COPIED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	COMMAND
		${CMAKE_COMMAND} -DSRC=${COPIED_PACLETINFO} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	DEPENDS
		${PACLETINFO_SOURCE}
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)
list(APPEND COPIED_WL_PACLET_SOURCES ${COPIED_PACLETINFO})

#
# static Kernel sources
#
foreach(SRC ${STATIC_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
			${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		DEPENDS
			${SRC}
			${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL})
endforeach()

#
# static Resources sources
#
foreach(SRC ${STATIC_WL_PACLET_RESOURCES_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Resources/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL}
		#
		# Do not inspect Resources sources
		#
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL})
endforeach()

#
# generated Kernel sources
#
foreach(SRC ${GENERATED_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_BINARY_DIR}/generated/wl/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL})
endforeach()

#
# generated Resources sources
#
foreach(SRC ${GENERATED_WL_PACLET_RESOURCES_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_BINARY_DIR}/generated/wl/Resources/ ${SRC})
	add_custom_command(
		OUTPUT
			${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL}
		COMMAND
			${CMAKE_COMMAND} -E copy ${SRC} ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL}
		DEPENDS
			${SRC}
	)
	list(APPEND COPIED_WL_PACLET_SOURCES ${PROJECT_BINARY_DIR}/paclet/${PACLET}/Resources/${REL})
endforeach()



#
# generated srcs
#

add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/generated/cpp/include/LongNames.h
		${PROJECT_BINARY_DIR}/generated/cpp/src/lib/LongNames.cpp
		${PROJECT_BINARY_DIR}/generated/wl/Resources/Generated/LongNames.wl
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/LongNames.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PROJECT_BINARY_DIR}/generated/wl/Resources/Generated/LongNames.wl -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Data/LongNames.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/LongNames.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)


set(GENERATED_TOKENENUM_OUTPUT
	${PROJECT_BINARY_DIR}/generated/cpp/include/TokenEnum.h
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/TokenEnum.cpp
)

add_custom_command(
	OUTPUT
		${GENERATED_TOKENENUM_OUTPUT}
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/TokenEnum.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Data/TokenEnum.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/TokenEnum.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)


set(GENERATED_PARSELETREGISTRATION_OUTPUT
	${PROJECT_BINARY_DIR}/generated/cpp/include/ParseletRegistration.h
	${PROJECT_BINARY_DIR}/generated/cpp/src/lib/ParseletRegistration.cpp
)

add_custom_command(
	OUTPUT
		${GENERATED_PARSELETREGISTRATION_OUTPUT}
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/ParseletRegistration.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Data/InfixParselets.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Data/PrefixParselets.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/ParseletRegistration.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${GENERATED_TOKENENUM_OUTPUT}
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/generated/cpp/include/Precedence.h
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/Precedence.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Data/Precedence.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Precedence.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/generated/wl/Kernel/RowBox.wl
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/RowBox.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
		${CMAKE_COMMAND} -DSRC=${PROJECT_BINARY_DIR}/generated/wl/Kernel/RowBox.wl -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Data/InfixParselets.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Data/PrefixParselets.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/RowBox.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
		${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

add_custom_command(
	OUTPUT
		${PROJECT_BINARY_DIR}/generated/cpp/include/Symbol.h
		${PROJECT_BINARY_DIR}/generated/cpp/src/lib/Symbol.cpp
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/Symbol.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Symbol.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${GENERATED_PARSELETREGISTRATION_OUTPUT}
		${GENERATED_TOKENENUM_OUTPUT}
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)


#
# expr lib target
#
if(BUILD_EXPR_LIB)

set(EXPR_LIB ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/expr${CMAKE_SHARED_LIBRARY_SUFFIX})

set(EXPR_LIB_OUTPUTS
	${EXPR_LIB}
)

add_library(expr-lib SHARED IMPORTED)

set_property(
	TARGET
		expr-lib
	PROPERTY
	IMPORTED_LOCATION
		${EXPR_LIB}
)


#
# Handle the import library stuff that is specific to Windows
#
if(CMAKE_HOST_WIN32 OR CYGWIN)

set(EXPR_IMPLIB ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/expr.lib)

list(APPEND EXPR_LIB_OUTPUTS
	${EXPR_IMPLIB}
)

set_property(
	TARGET
		expr-lib
	PROPERTY
	IMPORTED_IMPLIB
		${EXPR_IMPLIB}
)
endif(CMAKE_HOST_WIN32 OR CYGWIN)


add_custom_target(expr-lib-custom
	DEPENDS
		${EXPR_LIB_OUTPUTS}
)

add_custom_command(
	OUTPUT
		${EXPR_LIB_OUTPUTS}
	COMMAND
		${CMAKE_COMMAND} -E echo "EXPR_LIB_OUTPUTS: ${EXPR_LIB_OUTPUTS}"
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/ExprLibrary.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/ExprLibrary.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
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

set(STATIC_CPP_INCLUDES
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
list(APPEND STATIC_CPP_INCLUDES
	${PROJECT_SOURCE_DIR}/cpp/include/ExprLibrary.h
)
endif()



set(STATIC_CPP_LIB_SOURCES
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
	${PROJECT_SOURCE_DIR}/cpp/src/lib/UnderParselet.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/Utils.cpp
	${PROJECT_SOURCE_DIR}/cpp/src/lib/WLCharacter.cpp
)

if(BUILD_EXPR_LIB)
list(APPEND STATIC_CPP_LIB_SOURCES
${PROJECT_SOURCE_DIR}/cpp/src/lib/ExprLibrary.cpp
)
endif()



add_library(codeparser-lib SHARED
	${STATIC_CPP_INCLUDES}
	${STATIC_CPP_LIB_SOURCES}
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
# paclet target
#

set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
	codeparser-lib
)

if(BUILD_EXPR_LIB)
list(APPEND PACLET_SOURCES
	expr-lib
)
endif()

if(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${LOCAL_BUILD_VERSION}.paclet)
else(LOCAL_BUILD)
set(PACLET_OUTPUT ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet)
endif(LOCAL_BUILD)

add_custom_target(paclet
	DEPENDS
		${PACLET_OUTPUT}
)

#
# Create the paclet archive
#
add_custom_command(
	OUTPUT
		${PACLET_OUTPUT}
	COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET=${PACLET} -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	DEPENDS
		${PACLET_SOURCES}
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	VERBATIM
	WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
)

install(
	CODE
		"execute_process(COMMAND ${CMAKE_COMMAND} -DPACLET_OUTPUT=${PACLET_OUTPUT} \"-DWOLFRAMKERNEL=${WOLFRAMKERNEL}\" -DBUG349779_TIMEOUT=${BUG349779_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InstallPaclet.cmake)"
	COMPONENT
		paclet
)
