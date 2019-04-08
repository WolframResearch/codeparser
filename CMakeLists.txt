
cmake_minimum_required(VERSION 3.0)

project(ast)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

set(PACLET "AST")
set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")
set(BUILD_DOCS OFF CACHE BOOL "Build documentation")
set(BUILD_EXE OFF CACHE BOOL "Build executable")

message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")
message(STATUS "BUILD_EXE: ${BUILD_EXE}")

set(WL_SOURCES
	${CMAKE_SOURCE_DIR}/AST/Kernel/Abstract.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/AST.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Boxes.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/DeclarationName.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Node.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Symbol.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/ToInputFormString.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/ToFullFormString.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Utils.wl
)

set(PACLETINFO_SOURCE
	${CMAKE_SOURCE_DIR}/AST/PacletInfo.m
)

set(WL_DATA_SOURCES
	${CMAKE_SOURCE_DIR}/tables/CommaLongNames.wl
	${CMAKE_SOURCE_DIR}/tables/LongNames.wl
	${CMAKE_SOURCE_DIR}/tables/NewlineLongNames.wl
	${CMAKE_SOURCE_DIR}/tables/OperatorLongNames.wl
	${CMAKE_SOURCE_DIR}/tables/Precedence.wl
	${CMAKE_SOURCE_DIR}/tables/SpaceLongNames.wl
	${CMAKE_SOURCE_DIR}/tables/TokenEnum.wl
	${CMAKE_SOURCE_DIR}/tables/UninterpretableLongNames.wl
)

set(GENERATED_WL_SOURCES
	# not used right now ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/LongNameDefines.wl
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/Precedence.wl
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/Token.wl
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/LongNameReplacements.wl
)

set(GENERATED_WL_DATA_SOURCES
	${CMAKE_BINARY_DIR}/generated/wl/LongNameMap.wl
)

set(GENERATED_CPP_INCLUDES
	${CMAKE_BINARY_DIR}/generated/cpp/include/LongNameDefines.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/LongNameMap.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/Token.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/Precedence.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/CodePoint.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/Symbol.h
)

set(GENERATED_CPP_LIB_SOURCES
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/LongNameMap.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/Token.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/CodePoint.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/Symbol.cpp
)

set(GENERATED_SOURCES
	${GENERATED_WL_SOURCES}
	${GENERATED_WL_DATA_SOURCES}
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_LIB_SOURCES}
)


set(DOCUMENTATION_NOTEBOOK_PATHS
	English/ReferencePages/Symbols/ConcreteParseFile.nb
	English/ReferencePages/Symbols/ConcreteParseString.nb
	English/ReferencePages/Symbols/ParseFile.nb
	English/ReferencePages/Symbols/ParseString.nb
	English/ReferencePages/Symbols/TokenizeFile.nb
	English/ReferencePages/Symbols/TokenizeString.nb
)

#
# Setup documentation notebook paths
#
foreach(NB_PATH ${DOCUMENTATION_NOTEBOOK_PATHS})
	set(SRC ${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/${NB_PATH})
	set(BUILT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/${NB_PATH})
	list(APPEND DOCUMENTATION_SOURCE ${SRC})
	list(APPEND BUILT_DOCUMENTATION ${BUILT})
endforeach()



#
# Set VERSION_NUMBER, SYSTEMID, and PACLET_VERSION
#
include(WolframKernel)
CheckWolframKernel()



file(MAKE_DIRECTORY
	${CMAKE_BINARY_DIR}/paclet/${PACLET}
)


#
# Configure WL source files
#
# Use configure_file to create a file-level dependency between input and output
#
set(CONFIGURED_SRC ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)
configure_file(${PACLETINFO_SOURCE}
	${CONFIGURED_SRC}
)
list(APPEND CONFIGURED_WL_SOURCES ${CONFIGURED_SRC})
foreach(SRC ${WL_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	set(CONFIGURED_SRC ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
	configure_file(${SRC}
		${CONFIGURED_SRC}
	)
	list(APPEND CONFIGURED_WL_SOURCES ${CONFIGURED_SRC})
endforeach()

set(PACLET_SOURCES
	${CONFIGURED_WL_SOURCES}
	${GENERATED_WL_SOURCES}
	ast-lib
)

if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION}
)

endif(BUILD_DOCS)




#
# Generate sources
#
# Run the Generate.wl script to generate additional required C++ and WL files
#
add_custom_command(
	OUTPUT
		${GENERATED_SOURCES}
	DEPENDS
		${CMAKE_SOURCE_DIR}/scripts/Generate.wl
		${WL_DATA_SOURCES}
		# it may be overkill to depend on all WL sources, but it keeps things simple
		${WL_SOURCES}
		${PACLETINFO_SOURCE}
		${BUILT_PACLETINFO}
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/Generate.wl -buildDir ${CMAKE_BINARY_DIR}
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)




#
# Build library
#

set(WOLFRAMLIBRARY_INCLUDE_DIR /Applications/Mathematica.app/Contents/SystemFiles/IncludeFiles/C CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

set(MATHLINK_INCLUDE_DIR /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions CACHE FILEPATH "Path to mathlink.h")

message(STATUS "MATHLINK_INCLUDE_DIR: ${MATHLINK_INCLUDE_DIR}")

set(MATHLINK_LIB_DIR /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions CACHE FILEPATH "Path to MathLink library")

message(STATUS "MATHLINK_LIB_DIR: ${MATHLINK_LIB_DIR}")

set(CPP_INCLUDES
	${CMAKE_SOURCE_DIR}/cpp/include/API.h
	${CMAKE_SOURCE_DIR}/cpp/include/ByteDecoder.h
	${CMAKE_SOURCE_DIR}/cpp/include/ByteEncoder.h
	${CMAKE_SOURCE_DIR}/cpp/include/CharacterDecoder.h
	${CMAKE_SOURCE_DIR}/cpp/include/Node.h
	${CMAKE_SOURCE_DIR}/cpp/include/Parselet.h
	${CMAKE_SOURCE_DIR}/cpp/include/Parser.h
	${CMAKE_SOURCE_DIR}/cpp/include/Source.h
	${CMAKE_SOURCE_DIR}/cpp/include/Tokenizer.h
	${CMAKE_SOURCE_DIR}/cpp/include/Utils.h
)

set(CPP_LIB_SOURCES
	${CMAKE_SOURCE_DIR}/cpp/src/lib/API.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/ByteDecoder.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/ByteEncoder.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/CharacterDecoder.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Node.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Parselet.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Parser.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/SourceManager.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/SyntaxIssue.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Tokenizer.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Utils.cpp
)

add_library(ast-lib SHARED
	${CPP_INCLUDES}
	${CPP_LIB_SOURCES}
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_LIB_SOURCES}
)

target_include_directories(ast-lib
	PRIVATE ${CMAKE_SOURCE_DIR}/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
	PRIVATE ${MATHLINK_INCLUDE_DIR}
	PRIVATE ${WOLFRAMLIBRARY_INCLUDE_DIR}
)

find_library(MATHLINK_LIB
	#
	# mathlink is the mathlink.framework on MacOSX
	#
	NAMES mathlink ml64i4 ml32i4 ML64i4
	HINTS ${MATHLINK_LIB_DIR}
)

message(STATUS "MATHLINK_LIB: ${MATHLINK_LIB}")

target_link_libraries(ast-lib ${MATHLINK_LIB})


set(MAX_EXPRESSION_DEPTH 100 CACHE STRING "Maximum depth allowed by the parser")
set(MAX_EXPRESSION_BREADTH 100000 CACHE STRING "Maximum breadth allowed by the parser")
set(MAX_DIGITS_LENGTH 1000000 CACHE STRING "Maximum length of digits allowed by the parser")
set(MAX_PRECISION_LENGTH 6 CACHE STRING "Maximum length of precision allowed by the parser")
set(MAX_ACCURACY_LENGTH 6 CACHE STRING "Maximum length of accuracy allowed by the parser")
set(MAX_SYMBOL_LENGTH 1000000 CACHE STRING "Maximum length of symbols allowed by the parser")
set(MAX_STRING_LENGTH 1000000 CACHE STRING "Maximum length of strings allowed by the parser")

message(STATUS "MAX_EXPRESSION_DEPTH: ${MAX_EXPRESSION_DEPTH}")
message(STATUS "MAX_EXPRESSION_BREADTH: ${MAX_EXPRESSION_BREADTH}")
message(STATUS "MAX_DIGITS_LENGTH: ${MAX_DIGITS_LENGTH}")
message(STATUS "MAX_PRECISION_LENGTH: ${MAX_PRECISION_LENGTH}")
message(STATUS "MAX_ACCURACY_LENGTH: ${MAX_ACCURACY_LENGTH}")
message(STATUS "MAX_SYMBOL_LENGTH: ${MAX_SYMBOL_LENGTH}")
message(STATUS "MAX_STRING_LENGTH: ${MAX_STRING_LENGTH}")

target_compile_definitions(ast-lib
	PRIVATE MAX_EXPRESSION_DEPTH=${MAX_EXPRESSION_DEPTH}
	PRIVATE MAX_EXPRESSION_BREADTH=${MAX_EXPRESSION_BREADTH}
	PRIVATE MAX_DIGITS_LENGTH=${MAX_DIGITS_LENGTH}
	PRIVATE MAX_PRECISION_LENGTH=${MAX_PRECISION_LENGTH}
	PRIVATE MAX_ACCURACY_LENGTH=${MAX_ACCURACY_LENGTH}
	PRIVATE MAX_SYMBOL_LENGTH=${MAX_SYMBOL_LENGTH}
	PRIVATE MAX_STRING_LENGTH=${MAX_STRING_LENGTH}
)

set_target_properties(ast-lib PROPERTIES
	OUTPUT_NAME
		AST
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#library-output-artifacts
	#
	LIBRARY_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	#
	LIBRARY_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	LIBRARY_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#archive-output-artifacts
	#
	ARCHIVE_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	ARCHIVE_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# https://cmake.org/cmake/help/latest/manual/cmake-buildsystem.7.html#runtime-output-artifacts
	#
	RUNTIME_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	RUNTIME_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/LibraryResources/${SYSTEMID}
	#
	# WL / LibraryLink style dictates that the library be named "AST.dylib" and not "libAST.dylib"
	#
	PREFIX
		""
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(ast-lib
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else(MSVC)
	target_compile_options(ast-lib
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)



if(BUILD_EXE)

add_subdirectory(cpp/src/exe)

endif(BUILD_EXE)




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
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/ -DoutputDir=${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)

endif(BUILD_DOCS)






#
# paclet target
#
add_custom_target(paclet
	DEPENDS ${CMAKE_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
)

#
# Pack the paclet
#
add_custom_command(
	OUTPUT
		${CMAKE_BINARY_DIR}/paclet/${PACLET}-${PACLET_VERSION}.paclet
	DEPENDS
		${PACLET_SOURCES}
	COMMAND
		${WOLFRAMKERNEL} -noinit -noprompt -run PackPaclet["${CMAKE_BINARY_DIR}/paclet/${PACLET}"]\;Exit[]
	VERBATIM
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)



