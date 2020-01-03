
cmake_minimum_required(VERSION 3.0)

project(ast)

set(CMAKE_MODULE_PATH "${PROJECT_SOURCE_DIR}/cmake" ${CMAKE_MODULE_PATH})

set(PACLET "AST")
set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")
set(BUILD_DOCS OFF CACHE BOOL "Build documentation")
set(BUILD_EXE OFF CACHE BOOL "Build executable")
set(BUILD_EXPR_LIB ON CACHE BOOL "Build expr library")
set(USE_MATHLINK ON CACHE BOOL "Use MathLink")
set(NISSUES OFF CACHE BOOL "NISSUES")
set(NABORT OFF CACHE BOOL "NABORT")

message(STATUS "PACLET: ${PACLET}")
message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")
message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")
message(STATUS "BUILD_EXE: ${BUILD_EXE}")
message(STATUS "BUILD_EXPR_LIB: ${BUILD_EXPR_LIB}")
message(STATUS "USE_MATHLINK: ${USE_MATHLINK}")
message(STATUS "NISSUES: ${NISSUES}")
message(STATUS "NABORT: ${NABORT}")

set(STATIC_WL_PACLET_SOURCES
	${CMAKE_SOURCE_DIR}/AST/Kernel/Abstract.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/AST.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Boxes.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/CodeAction.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/DeclarationName.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Folds.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Library.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Node.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Quirks.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Shims.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/ToString.wl
	${CMAKE_SOURCE_DIR}/AST/Kernel/Utils.wl
)

set(GENERATED_WL_PACLET_SOURCES
	${CMAKE_BINARY_DIR}/generated/wl/LongNames.wl
)

set(WL_PACLET_SOURCES
	${STATIC_WL_PACLET_SOURCES}
	${GENERATED_WL_PACLET_SOURCES}
)

#
# Used during the build process
#
set(WL_GENERATE_SOURCES
	${CMAKE_SOURCE_DIR}/AST/Generate/Generate.wl
	${CMAKE_SOURCE_DIR}/AST/Generate/CharacterMaps.wl
	${CMAKE_SOURCE_DIR}/AST/Generate/CodePoint.wl
	${CMAKE_SOURCE_DIR}/AST/Generate/LongNames.wl
	${CMAKE_SOURCE_DIR}/AST/Generate/Precedence.wl
	${CMAKE_SOURCE_DIR}/AST/Generate/Symbol.wl
	${CMAKE_SOURCE_DIR}/AST/Generate/TokenEnum.wl
)

set(WL_GENERATE_EXPRLIB_SOURCES
	${CMAKE_SOURCE_DIR}/AST/Generate/ExpressionLibrary.wl
)

set(PACLETINFO_SOURCE
	${CMAKE_SOURCE_DIR}/AST/PacletInfo.m
)

set(WL_DATA_SOURCES
	${CMAKE_SOURCE_DIR}/tables/LongNames.wl
	${CMAKE_SOURCE_DIR}/tables/Precedence.wl
	${CMAKE_SOURCE_DIR}/tables/TokenEnum.wl
)

set(GENERATED_CPP_INCLUDES
	${CMAKE_BINARY_DIR}/generated/cpp/include/LongNameDefines.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/Precedence.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/Symbol.h
	${CMAKE_BINARY_DIR}/generated/cpp/include/TokenEnum.h
)

set(GENERATED_CPP_LIB_SOURCES
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/CharacterMaps.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/CodePoint.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/Symbol.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/lib/TokenEnum.cpp
)

set(GENERATED_SOURCES
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_LIB_SOURCES}
	${GENERATED_WL_PACLET_SOURCES}
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
# Copy WL source files
#
add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m
    COMMAND ${CMAKE_COMMAND} -E copy ${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m
    MAIN_DEPENDENCY ${CMAKE_SOURCE_DIR}/${PACLET}/PacletInfo.m)
list(APPEND COPIED_WL_PACLET_SOURCES ${CMAKE_BINARY_DIR}/paclet/${PACLET}/PacletInfo.m)

foreach(SRC ${WL_PACLET_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	add_custom_command(
    OUTPUT ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    COMMAND ${CMAKE_COMMAND} -E copy ${SRC} ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC}
    MAIN_DEPENDENCY ${SRC})
	list(APPEND COPIED_WL_PACLET_SOURCES ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Kernel/${BARE_SRC})
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
# Run the Generate.wl script to generate additional required source files
#
add_custom_command(
	OUTPUT
		${GENERATED_SOURCES}
	DEPENDS
		${WL_DATA_SOURCES}
		${WL_GENERATE_SOURCES}
	COMMAND
		${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/AST/Generate/Generate.wl -buildDir ${CMAKE_BINARY_DIR}
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)






#
# expr lib target
#
if(${BUILD_EXPR_LIB})

set(EXPR_LIB ${CMAKE_BINARY_DIR}/paclet/AST/LibraryResources/${SYSTEMID}/expr${CMAKE_SHARED_LIBRARY_SUFFIX})

message(STATUS "EXPR_LIB: ${EXPR_LIB}")

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
	COMMAND
		${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/AST/Generate/ExpressionLibrary.wl -buildDir ${CMAKE_BINARY_DIR}
	VERBATIM
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)

add_dependencies(expr-lib expr-lib-custom)

endif(${BUILD_EXPR_LIB})





#
# Build AST library
#

set(WOLFRAMLIBRARY_INCLUDE_DIR /Applications/Mathematica.app/Contents/SystemFiles/IncludeFiles/C CACHE FILEPATH "Path to WolframLibrary.h")

message(STATUS "WOLFRAMLIBRARY_INCLUDE_DIR: ${WOLFRAMLIBRARY_INCLUDE_DIR}")

if(USE_MATHLINK)

set(MATHLINK_INCLUDE_DIR /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions CACHE FILEPATH "Path to mathlink.h")

message(STATUS "MATHLINK_INCLUDE_DIR: ${MATHLINK_INCLUDE_DIR}")

set(MATHLINK_LIB_DIR /Applications/Mathematica.app/Contents/SystemFiles/Links/MathLink/DeveloperKit/MacOSX-x86-64/CompilerAdditions CACHE FILEPATH "Path to MathLink library")

message(STATUS "MATHLINK_LIB_DIR: ${MATHLINK_LIB_DIR}")

endif(USE_MATHLINK)

set(CPP_INCLUDES
	${CMAKE_SOURCE_DIR}/cpp/include/API.h
	${CMAKE_SOURCE_DIR}/cpp/include/ByteBuffer.h
	${CMAKE_SOURCE_DIR}/cpp/include/ByteDecoder.h
	${CMAKE_SOURCE_DIR}/cpp/include/ByteEncoder.h
	${CMAKE_SOURCE_DIR}/cpp/include/CharacterDecoder.h
	${CMAKE_SOURCE_DIR}/cpp/include/CharacterMaps.h
	${CMAKE_SOURCE_DIR}/cpp/include/CodePoint.h
	${CMAKE_SOURCE_DIR}/cpp/include/Node.h
	${CMAKE_SOURCE_DIR}/cpp/include/Parselet.h
	${CMAKE_SOURCE_DIR}/cpp/include/Parser.h
	${CMAKE_SOURCE_DIR}/cpp/include/Source.h
	${CMAKE_SOURCE_DIR}/cpp/include/Token.h
	${CMAKE_SOURCE_DIR}/cpp/include/Tokenizer.h
	${CMAKE_SOURCE_DIR}/cpp/include/Utils.h
	${CMAKE_SOURCE_DIR}/cpp/include/WLCharacter.h
)

if(${BUILD_EXPR_LIB})
list(APPEND CPP_INCLUDES
    ${CMAKE_SOURCE_DIR}/cpp/include/ExpressionLibrary.h
)
endif()



set(CPP_LIB_SOURCES
	${CMAKE_SOURCE_DIR}/cpp/src/lib/API.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/ByteBuffer.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/ByteDecoder.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/ByteEncoder.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/CharacterDecoder.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Node.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Parselet.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Parser.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/SemiSemiParselet.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Source.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Token.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Tokenizer.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/Utils.cpp
	${CMAKE_SOURCE_DIR}/cpp/src/lib/WLCharacter.cpp
)

if(${BUILD_EXPR_LIB})
list(APPEND CPP_LIB_SOURCES
${CMAKE_SOURCE_DIR}/cpp/src/lib/ExpressionLibrary.cpp
)
endif()



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



if(USE_MATHLINK)

find_library(MATHLINK_LIB
	#
	# mathlink is the mathlink.framework on MacOSX
	#
	NAMES mathlink ml64i4 ml32i4 ML64i4
	HINTS ${MATHLINK_LIB_DIR}
)

message(STATUS "MATHLINK_LIB: ${MATHLINK_LIB}")

target_link_libraries(ast-lib ${MATHLINK_LIB})

endif(USE_MATHLINK)

if(${BUILD_EXPR_LIB})
target_link_libraries(ast-lib expr-lib)
endif()

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


if(NISSUES)
target_compile_definitions(ast-lib PUBLIC NISSUES=1)
endif(NISSUES)

if(NABORT)
target_compile_definitions(ast-lib PUBLIC NABORT=1)
endif(NABORT)

if(USE_MATHLINK)
target_compile_definitions(ast-lib PUBLIC USE_MATHLINK=1)
endif(USE_MATHLINK)



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
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${CMAKE_SOURCE_DIR}/${PACLET}/Documentation/ -DoutputDir=${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)

endif(BUILD_DOCS)




#
# paclet target
#

set(PACLET_SOURCES
	${COPIED_WL_PACLET_SOURCES}
	generated-srcs
	ast-lib
)

if(${BUILD_EXPR_LIB})
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



