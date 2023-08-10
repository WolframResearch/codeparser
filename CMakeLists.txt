#
# https://cmake.org/cmake/help/latest/release/3.17.html
# cmake(1) gained a -E rm command-line tool that can be used to remove directories and files. This supersedes the existing -E remove and -E remove_directory tools and has better semantics.
#
cmake_minimum_required(VERSION 3.17)

project(codeparser
	LANGUAGES
	CXX
)

include(ExternalProject)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})
include(WolframKernel)
include(PacletInfo)
include(WolframLibrary)

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
set(PACLET_LAYOUT_DIR "paclet" CACHE FILEPATH "Path to complete, built paclet layout (relative to build directory)")
set(WOLFRAMKERNEL ${WOLFRAMKERNEL_DEFAULT} CACHE FILEPATH "Path to WolframKernel")
set(BUILD_PACLET ON CACHE BOOL "Build paclet")
set(TRANSPORT Automatic CACHE STRING "Transport to use")
set(CHECK_ISSUES ON CACHE BOOL "CHECK_ISSUES")
set(CHECK_ABORT ON CACHE BOOL "CHECK_ABORT")
set(COMPUTE_OOB ON CACHE BOOL "COMPUTE_OOB")
set(COMPUTE_SOURCE ON CACHE BOOL "COMPUTE_SOURCE")
set(FAST_STRING_SCAN OFF CACHE BOOL "FAST_STRING_SCAN")
set(USE_MUSTTAIL OFF CACHE BOOL "USE_MUSTTAIL")
set(DIAGNOSTICS OFF CACHE BOOL "DIAGNOSTICS")
set(LOCAL_BUILD OFF CACHE BOOL "Local build")

#
# Time to pause when starting kernel
#
# Work-around for bug 349779 is to pause ~1 second
# bug 349779 was fixed in version 12.0
#
# Related bugs: 349779
#
set(KERNEL_PAUSE 0 CACHE STRING "Kernel startup pause")

#
# Timeout for starting a kernel and getting a result
#
# RE machines can be very slow when starting a kernel, so we need to be very generous with this timeout
#
# Should be at least 10 minutes = 600 seconds
#
# Evidence suggests that when bug 349779 strikes, the kernel does exit after 30 minutes = 1800 seconds
# bug 349779 was fixed in version 12.0
#
# Related bugs: 349779
# Related issues: RE-514227
#
set(KERNEL_TIMEOUT 600 CACHE STRING "Kernel startup timeout")

if(NOT DEFINED BUILDNUMBER)
	set(BUILDNUMBER 0)
endif()

message(STATUS "BUILDNUMBER: ${BUILDNUMBER}")
message(STATUS "CMAKE_VERSION: ${CMAKE_VERSION}")
message(STATUS "PACLET: ${PACLET}")
message(STATUS "PACLET_LAYOUT_DIR: ${PACLET_LAYOUT_DIR}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_PACLET: ${BUILD_PACLET}")
message(STATUS "TRANSPORT: ${TRANSPORT}")
message(STATUS "CHECK_ISSUES: ${CHECK_ISSUES}")
message(STATUS "CHECK_ABORT: ${CHECK_ABORT}")
message(STATUS "COMPUTE_OOB: ${COMPUTE_OOB}")
message(STATUS "COMPUTE_SOURCE: ${COMPUTE_SOURCE}")
message(STATUS "FAST_STRING_SCAN: ${FAST_STRING_SCAN}")
message(STATUS "USE_MUSTTAIL: ${USE_MUSTTAIL}")
message(STATUS "DIAGNOSTICS: ${DIAGNOSTICS}")
message(STATUS "LOCAL_BUILD: ${LOCAL_BUILD}")

if(LOCAL_BUILD)
	message(STATUS "Configuring for local build")
endif()

message(STATUS "CMAKE_SIZEOF_VOID_P: ${CMAKE_SIZEOF_VOID_P}")
message(STATUS "KERNEL_PAUSE: ${KERNEL_PAUSE}")
message(STATUS "KERNEL_TIMEOUT: ${KERNEL_TIMEOUT}")
message(STATUS "CODEPARSER_EXE: ${CODEPARSER_EXE}")

if(NOT CODEPARSER_EXE)
	message(STATUS "Optional tool CODEPARSER_EXE was not found; skipping")
endif()

set(WOLFRAMLIBRARY_INCLUDE_DIR ${WOLFRAMLIBRARY_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

ParseWolframLibraryHeader()

message(STATUS "WOLFRAMLIBRARY_VERSION: ${WOLFRAMLIBRARY_VERSION}")

if(WOLFRAMLIBRARY_VERSION GREATER_EQUAL 7)
	message(WARNING "WOLFRAMLIBRARY_VERSION >= 7 is only compatible with Wolfram System 13.1+ and will not work with earlier versions.")
endif()

set(STATIC_WL_PACLET_KERNEL_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Abstract.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/CodeParser.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Boxes.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/CodeAction.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Concretify.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Definitions.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Folds.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Library.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Node.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Quirks.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Scoping.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Shims.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Trees.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/TopLevel.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/ToString.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/TokenEnum.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Kernel/Utils.wl
)

set(STATIC_WL_PACLET_RESOURCES_SOURCES
	${PROJECT_SOURCE_DIR}/CodeParser/Resources/Examples/Collatz.m
)

set(PACLETINFO_IN_SOURCE
	${PROJECT_SOURCE_DIR}/${PACLET}/PacletInfo.wl.in
)

set(GENERATED_WL_PACLET_KERNEL_SOURCES
	${PROJECT_BINARY_DIR}/paclet/CodeParser/Kernel/RowBox.wl
)

set(GENERATED_WL_PACLET_RESOURCES_SOURCES
	${PROJECT_BINARY_DIR}/paclet/CodeParser/Resources/Generated/LongNames.wl
	${PROJECT_BINARY_DIR}/paclet/CodeParser/Resources/Generated/Precedence.wl
)

#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
CheckWolframKernel()
CheckPacletInfo()

if(TRANSPORT STREQUAL Automatic)
	if(VERSION_NUMBER GREATER_EQUAL 1310)
		# TODO: I've reset the default transport to MathLink during the port to Rust.
		# Change this back to ExprLib once that code is ported implemented.
		# set(TRANSPORT ExprLib)
		set(TRANSPORT MathLink)
	else()
		set(TRANSPORT MathLink)
	endif()

	message(STATUS "resolved TRANSPORT: ${TRANSPORT}")
endif()

#
# Force re-configure if PacletInfo.wl.in changes, e.g. paclet version is changed and name of .paclet has changed
#
set_property(
	DIRECTORY
	APPEND
	PROPERTY
	CMAKE_CONFIGURE_DEPENDS
	${PACLETINFO_IN_SOURCE}
)

if(NOT VERSION_NUMBER GREATER_EQUAL 1210)
	message(WARNING "VERSION_NUMBER is below 1210; ForceVersionInstall was added in 12.1. Installing paclets via CMake may not work. (VERSION_NUMBER is ${VERSION_NUMBER})")
endif()

if(TRANSPORT STREQUAL ExprLib)
	# if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1200)
	# message(FATAL_ERROR "VERSION_NUMBER is below 1200; Expr library uses NumericArrays that were added to LibraryLink in version 12.0. (VERSION_NUMBER is ${VERSION_NUMBER})")
	# endif()
	if(NOT VERSION_NUMBER GREATER_EQUAL 1310)
		message(FATAL_ERROR "VERSION_NUMBER is below 1310; Cannot build Expr library before version 13.1. (VERSION_NUMBER is ${VERSION_NUMBER})")
	endif()
endif()

if(APPLE)
	include(MacOSXVersionMin)
	CheckMacOSXVersionMin()

	if(NOT MACOSX_VERSION_MIN STREQUAL CMAKE_OSX_DEPLOYMENT_TARGET)
		message(WARNING
			"MACOSX_VERSION_MIN: ${MACOSX_VERSION_MIN}\n"
			"CMAKE_OSX_DEPLOYMENT_TARGET: ${CMAKE_OSX_DEPLOYMENT_TARGET}\n"
			"MACOSX_VERSION_MIN and CMAKE_OSX_DEPLOYMENT_TARGET do not match\n"
			"HINT: Specify -DCMAKE_OSX_DEPLOYMENT_TARGET=${MACOSX_VERSION_MIN} to ensure CodeParser is built with same MacOSXVersionMin as CCompilerDriver libraries.")
	endif()
endif()

file(MAKE_DIRECTORY
	${PROJECT_BINARY_DIR}/paclet/${PACLET}
	${PROJECT_BINARY_DIR}/pdb-output/
)

#
# Copy WL source files
#
set(REPLACED_PACLETINFO ${PROJECT_BINARY_DIR}/paclet/${PACLET}/PacletInfo.wl)

add_custom_command(
	OUTPUT
	${REPLACED_PACLETINFO}
	COMMAND
	${CMAKE_COMMAND} -DSRC=${PACLETINFO_IN_SOURCE} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	COMMAND
	${CMAKE_COMMAND} -DTRANSPORT=${TRANSPORT} -DBUILDNUMBER=${BUILDNUMBER} -DVERSION_NUMBER=${VERSION_NUMBER} -DWOLFRAMLIBRARY_VERSION=${WOLFRAMLIBRARY_VERSION} -DLOCAL_BUILD=${LOCAL_BUILD} -DLOCAL_BUILD_VERSION=${LOCAL_BUILD_VERSION} -DPACLETINFO_IN_SOURCE=${PACLETINFO_IN_SOURCE} -DREPLACED_PACLETINFO=${REPLACED_PACLETINFO} -P ${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
	DEPENDS
	${PACLETINFO_IN_SOURCE}
	${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	${PROJECT_SOURCE_DIR}/cmake/ReplacePacletInfo.cmake
)

#
# static Kernel sources
#
foreach(SRC ${STATIC_WL_PACLET_KERNEL_SOURCES})
	file(RELATIVE_PATH REL ${PROJECT_SOURCE_DIR}/${PACLET}/Kernel/ ${SRC})
	add_custom_command(
		OUTPUT
		${PROJECT_BINARY_DIR}/paclet/${PACLET}/Kernel/${REL}
		COMMAND
		${CMAKE_COMMAND} -DSRC=${SRC} -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
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
# generated srcs
#

#
# LongNames files
#
add_custom_command(
	OUTPUT
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/generated/long_names_registration.rs
	${PROJECT_BINARY_DIR}/paclet/CodeParser/Resources/Generated/LongNames.wl

	COMMAND
	${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/LongNames.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
	${CMAKE_COMMAND} -DSRC=${PROJECT_BINARY_DIR}/paclet/CodeParser/Resources/Generated/LongNames.wl -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
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

#
# Precedence files
#
add_custom_command(
	OUTPUT
	${PROJECT_BINARY_DIR}/paclet/CodeParser/Resources/Generated/Precedence.wl
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/generated/precedence_values.rs
	COMMAND
	${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/Precedence.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
	${CMAKE_COMMAND} -DSRC=${PROJECT_BINARY_DIR}/paclet/CodeParser/Resources/Generated/Precedence.wl -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	DEPENDS
	${PROJECT_SOURCE_DIR}/CodeParser/Data/Precedence.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/Precedence.wl
	${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
	${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
	${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
	VERBATIM
	WORKING_DIRECTORY
	${PROJECT_SOURCE_DIR}
)

#
# RowBox files
#
add_custom_command(
	OUTPUT
	${PROJECT_BINARY_DIR}/paclet/CodeParser/Kernel/RowBox.wl
	COMMAND
	${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/RowBox.wl -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DSRCDIR=${PROJECT_SOURCE_DIR} -DBUILDDIR=${PROJECT_BINARY_DIR} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	COMMAND
	${CMAKE_COMMAND} -DSRC=${PROJECT_BINARY_DIR}/paclet/CodeParser/Kernel/RowBox.wl -DCODEPARSER_EXE=${CODEPARSER_EXE} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InspectFile.cmake
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

#
# expr lib target
#
if(TRANSPORT STREQUAL ExprLib)
	set(EXPR_LIB ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/libexpr${CMAKE_SHARED_LIBRARY_SUFFIX})

	set(EXPR_LIB_OUTPUTS
		${EXPR_LIB}
	)

	add_library(expr-lib SHARED IMPORTED)

	set_target_properties(expr-lib PROPERTIES
		IMPORTED_LOCATION
		${EXPR_LIB}

		# IMPORTED_NO_SONAME is meant only to tell CMake that a .so doesn't have a SONAME field.
		# If such a .so is passed by file path to the linker, some linkers will put the path to the .so in the NEEDED field of consumers.
		# As a workaround, CMake switches to linking via -l<name>, in which case the library must be found by the linker's normal search rules.
		# On UNIX platforms, that means it needs to be named lib<name>.so.
		# https://gitlab.kitware.com/cmake/cmake/-/issues/22703
		IMPORTED_NO_SONAME
		ON
	)

	if(WIN32)
		set(EXPR_IMPLIB ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/libexpr.lib)
		set(EXPR_EXP ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/libexpr.exp)

		set_property(
			TARGET
			expr-lib
			PROPERTY
			IMPORTED_IMPLIB
			${EXPR_IMPLIB}
		)

		list(APPEND EXPR_LIB_OUTPUTS
			${EXPR_IMPLIB}
			${EXPR_EXP}
		)
	endif(WIN32)

	add_custom_target(expr-lib-custom
		DEPENDS
		${EXPR_LIB_OUTPUTS}
	)

	add_custom_command(
		OUTPUT
		${EXPR_LIB_OUTPUTS}

		#
		# remove old files that may cause conflicts
		#
		COMMAND
		${CMAKE_COMMAND} -E rm -f ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/expr${CMAKE_SHARED_LIBRARY_SUFFIX}
		COMMAND
		${CMAKE_COMMAND} -E rm -f ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/expr.lib
		COMMAND
		${CMAKE_COMMAND} -E rm -f ${PROJECT_BINARY_DIR}/paclet/CodeParser/LibraryResources/${SYSTEMID}/expr.exp
		COMMAND
		${CMAKE_COMMAND} -DSCRIPT=${PROJECT_SOURCE_DIR}/${PACLET}/Generate/ExprLibrary.wl -DBUILDDIR=${PROJECT_BINARY_DIR} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
		DEPENDS
		${PROJECT_SOURCE_DIR}/cpp/include/ExprLibrary.h
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/ExprLibrary.wl
		${PROJECT_SOURCE_DIR}/CodeParser/Generate/Common.wl
		${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
		${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
		VERBATIM
		WORKING_DIRECTORY
		${PROJECT_SOURCE_DIR}
	)

	add_dependencies(expr-lib expr-lib-custom)
endif(TRANSPORT STREQUAL ExprLib)

#
# Build the Rust crate and copy the dylib into the paclet
#
set(RUST_TARGET_DIR ${PROJECT_SOURCE_DIR}/target)

# WL / LibraryLink style dictates that the library be named "CodeParser.dylib" and not "libCodeParser.dylib"
set(CODEPARSE_DYLIB ${PROJECT_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}/CodeParser${CMAKE_SHARED_LIBRARY_SUFFIX})

add_custom_command(
	OUTPUT
	${CODEPARSE_DYLIB}
	WORKING_DIRECTORY
	${PROJECT_SOURCE_DIR}

	COMMAND
	cd crates/codeparser-wll
	&&
	${CMAKE_COMMAND} -E env

	# ===== Environment variables
	# FIXME: Don't build using an app, use WSTP/WolframLibrary directly.
	# WSTP_COMPILER_ADDITIONS_DIRECTORY="${WSTP_LIB_DIR}"
	# WOLFRAM_LIBRARY_LINK_C_INCLUDES_DIRECTORY="${WOLFRAMLIBRARY_INCLUDE_DIR}"
	RUST_WOLFRAM_LOCATION="${MATHEMATICA_INSTALL_DIR}"
	WSTP_COMPILER_ADDITIONS_DIRECTORY="${MATHEMATICA_INSTALL_DIR}/SystemFiles/Links/WSTP/DeveloperKit/${SYSTEMID}/CompilerAdditions/"
	WOLFRAM_LIBRARY_LINK_C_INCLUDES_DIRECTORY="${MATHEMATICA_INSTALL_DIR}/SystemFiles/IncludeFiles/C"

	# Print 'trace'-level logging info, but disable bindgen logging,
	# which isn't typically useful, and is very verbose.
	RUST_LOG="trace,bindgen=off"

	# ===== Actual command
	cargo build
	$<$<CONFIG:Release>:--release>
	--features=USE_MATHLINK

	COMMAND
	${CMAKE_COMMAND} -E copy
	${RUST_TARGET_DIR}/$<IF:$<CONFIG:Release>,release,debug>/${CMAKE_SHARED_LIBRARY_PREFIX}codeparser_wll${CMAKE_SHARED_LIBRARY_SUFFIX}
	${CODEPARSE_DYLIB}

	DEPENDS
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/generated/precedence_values.rs
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/generated/long_names_registration.rs
	${PROJECT_SOURCE_DIR}/crates/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/abstract_/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/read/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/bin/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/tests/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/tokenize/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/parse/*
	${PROJECT_SOURCE_DIR}/crates/wolfram-parser/src/parse/parselet/*
	${PROJECT_SOURCE_DIR}/crates/codeparser-wll/*
	${PROJECT_SOURCE_DIR}/crates/codeparser-wll/src/*
)

# "Fake" named target that just runs the custom command to generate the
# CodeParser dynamic library.
add_custom_target(
	codeparser-lib
	DEPENDS
	${CODEPARSE_DYLIB}
)

#
# Build CodeParser library
#
if(TRANSPORT STREQUAL MathLink)
	set(MATHLINK_INCLUDE_DIR ${MATHLINK_INCLUDE_DIR_DEFAULT} CACHE FILEPATH "Path to mathlink.h")

	message(STATUS "MATHLINK_INCLUDE_DIR: ${MATHLINK_INCLUDE_DIR}")

	set(MATHLINK_LIB_DIR ${MATHLINK_LIB_DIR_DEFAULT} CACHE FILEPATH "Path to MathLink library")

	message(STATUS "MATHLINK_LIB_DIR: ${MATHLINK_LIB_DIR}")
endif(TRANSPORT STREQUAL MathLink)

if(TRANSPORT STREQUAL MathLink)
	#
	# Cannot just have all lib names together in a single list
	# because e.g., Linux may have both 32-bit and 64-bit libs present,
	# and 64-bit builds may happen to find the 32-bit version first and fail.
	#
	if(SYSTEMWORDLENGTH EQUAL 32)
		set(MATHLINK_LIB_NAMES
			ml32i4
			ML32i4
			ml32i4m
		)
	elseif(SYSTEMWORDLENGTH EQUAL 64)
		set(MATHLINK_LIB_NAMES

			#
			# mathlink is the mathlink.framework on MacOSX
			#
			mathlink
			ml64i4
			ML64i4

			#
			# in version 12.2: ml64i4m has become the (only) official name of the Windows import library
			# Related issues: RE-514316
			#
			ml64i4m
		)
	endif(SYSTEMWORDLENGTH EQUAL 32)

	find_library(MATHLINK_LIB
		NAMES
		${MATHLINK_LIB_NAMES}
		HINTS
		${MATHLINK_LIB_DIR}
	)

	message(STATUS "MATHLINK_LIB: ${MATHLINK_LIB}")

	set(CRATE_NAME wolfram-code-parse)
	set(CRATE_SOURCE_DIR ${PROJECT_SOURCE_DIR}/crate/)

	# target_link_libraries(codeparser-lib ${MATHLINK_LIB})
endif(TRANSPORT STREQUAL MathLink)

if(${TRANSPORT} STREQUAL ExprLib)
	# target_link_libraries(codeparser-lib expr-lib)
endif()

# set_target_properties(codeparser-lib PROPERTIES
# OUTPUT_NAME
# CodeParser
# #
# # prevent
# # /Volumes/TeamCity/work/4466f913884ff7b4/MathLink/CompilerAdditions
# # from being added to rpath
# #
# SKIP_BUILD_RPATH
# ON
# )

#[[ FIXME: Adjust this for the Rust CodeParser.dylib
if(TRANSPORT STREQUAL ExprLib)

#
# expr lib will be located in the same directory as CodeParser lib
#

#
# see page 100 of:
# https://github.com/CppCon/CppCon2019/blob/master/Presentations/deep_cmake_for_library_authors/deep_cmake_for_library_authors__craig_scott__cppcon_2019.pdf
#
if(NOT APPLE)
set_target_properties(codeparser-lib PROPERTIES
	INSTALL_RPATH
		$ORIGIN
	BUILD_WITH_INSTALL_RPATH
		ON
)
endif(NOT APPLE)

endif(TRANSPORT STREQUAL ExprLib)

]]

# if(USE_MUSTTAIL)
# target_compile_definitions(codeparser-lib PUBLIC USE_MUSTTAIL=1)
# else()
# target_compile_definitions(codeparser-lib PUBLIC USE_MUSTTAIL=0)
# endif()

# if(TRANSPORT STREQUAL MathLink)
# target_compile_definitions(codeparser-lib PUBLIC USE_MATHLINK=1)
# else()
# target_compile_definitions(codeparser-lib PUBLIC USE_MATHLINK=0)
# endif()

# if(TRANSPORT STREQUAL ExprLib)
# target_compile_definitions(codeparser-lib PUBLIC USE_EXPR_LIB=1)
# else()
# target_compile_definitions(codeparser-lib PUBLIC USE_EXPR_LIB=0)
# endif()

# if(DIAGNOSTICS)
# target_compile_definitions(codeparser-lib PUBLIC DIAGNOSTICS=1)
# else()
# target_compile_definitions(codeparser-lib PUBLIC DIAGNOSTICS=0)
# endif()

if(BUILD_PACLET)
	#
	# paclet layout
	#
	set(PACLET_SOURCES
		${REPLACED_PACLETINFO}

		# TODO(cleanup): Remove COPIED_WL_PACLET_SOURCES?
		${COPIED_WL_PACLET_SOURCES}
		${GENERATED_WL_PACLET_KERNEL_SOURCES}
		${GENERATED_WL_PACLET_RESOURCES_SOURCES}
		${CODEPARSE_DYLIB}
	)

	if(TRANSPORT STREQUAL ExprLib)
		list(APPEND PACLET_SOURCES
			expr-lib
		)
	endif()

	#
	# Assemble Paclet
	#

	add_custom_target(assemble-paclet
		DEPENDS
			${PACLET_SOURCES}
			${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl
			${PROJECT_SOURCE_DIR}/CodeTools/Generate/GenerateSources.wl
			${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
	)

	#
	# paclet archive
	#
	if(LOCAL_BUILD)
		set(PACLET_ARCHIVE ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${LOCAL_BUILD_VERSION}.paclet)
	else(LOCAL_BUILD)
		set(PACLET_ARCHIVE ${PROJECT_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet)
	endif(LOCAL_BUILD)

	add_custom_target(create-paclet-archive
		ALL
		DEPENDS
		${PACLET_ARCHIVE}
	)

	#
	# CreatePacletArchive
	#
	add_custom_command(
		OUTPUT
		${PACLET_ARCHIVE}
		COMMAND

		#
		# CreatePacletArchive may be slow on RE machines, so allow re-trying if JLink connection timeout is hit
		#
		# see: RE-515885
		#
		${CMAKE_COMMAND} -DRETRY_ON_FAILURE=ON -DSCRIPT=${PROJECT_SOURCE_DIR}/CodeTools/Generate/CreatePacletArchive.wl -DBUILDDIR=${PROJECT_BINARY_DIR} -DPACLET_LAYOUT_DIR=${PACLET_LAYOUT_DIR} -DPACLET=${PACLET} -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -DWOLFRAMKERNEL=${WOLFRAMKERNEL} -P ${PROJECT_SOURCE_DIR}/cmake/WolframScript.cmake
		DEPENDS
			assemble-paclet
		VERBATIM
		WORKING_DIRECTORY
		${PROJECT_BINARY_DIR}
	)

	#
	# PacletInstall
	#
	install(
		CODE
		"execute_process(COMMAND ${CMAKE_COMMAND} -DPACLET_ARCHIVE=${PACLET_ARCHIVE} -DPACLET_WOLFRAMVERSION=${PACLET_WOLFRAMVERSION} \"-DWOLFRAMKERNEL=${WOLFRAMKERNEL}\" -DKERNEL_TIMEOUT=${KERNEL_TIMEOUT} -P ${PROJECT_SOURCE_DIR}/cmake/InstallPaclet.cmake)"
		COMPONENT
		create-paclet-archive
	)
endif(BUILD_PACLET)
