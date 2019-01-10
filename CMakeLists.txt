
cmake_minimum_required(VERSION 3.0)

project(ast)

set(PACLET "AST")

message(STATUS "PACLET: ${PACLET}")

set(PACLET_VERSION "0.7" CACHE STRING "Paclet Version")

message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")

set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")

message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")

set(BUILD_DOCS OFF CACHE BOOL "Build documentation")

set(WL_SOURCES
	${CMAKE_SOURCE_DIR}/PacletInfo.m
	${CMAKE_SOURCE_DIR}/AST/Abstract.wl
	${CMAKE_SOURCE_DIR}/AST/AST.wl
	${CMAKE_SOURCE_DIR}/AST/DeclarationName.wl
	${CMAKE_SOURCE_DIR}/AST/Node.wl
	${CMAKE_SOURCE_DIR}/AST/Symbol.wl
	${CMAKE_SOURCE_DIR}/AST/ToInputFormString.wl
	${CMAKE_SOURCE_DIR}/AST/ToFullFormString.wl
	${CMAKE_SOURCE_DIR}/AST/Utils.wl
)

set(CPP_SOURCES
	${CMAKE_SOURCE_DIR}/AST/cpp/src/SourceManager.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/ByteDecoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/CharacterDecoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Tokenizer.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Parser.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Utils.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Parselet.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/Node.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/ToInputFormString.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/main.cpp
)

set(WL_DATA_SOURCES
	${CMAKE_SOURCE_DIR}/AST/tables/CommaLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/LetterlikeLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/LongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/NewlineLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/OperatorLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/Precedence.wl
	${CMAKE_SOURCE_DIR}/AST/tables/SpaceLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/StrangeLetterlikeCodePoints.wl
	${CMAKE_SOURCE_DIR}/AST/tables/TokenEnum.wl
)

set(GENERATED_WL_SOURCES
	# not used right now ${CMAKE_BINARY_DIR}/paclet/${PACLET}/LongNameDefines.wl
	# not used right now ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Precedence.wl
	# not used right now ${CMAKE_BINARY_DIR}/paclet/${PACLET}/Token.wl
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
	${CMAKE_BINARY_DIR}/generated/cpp/include/ToInputFormString.h
)

set(GENERATED_CPP_SOURCES
	${CMAKE_BINARY_DIR}/generated/cpp/src/LongNameMap.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/Token.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/CodePoint.cpp
	${CMAKE_BINARY_DIR}/generated/cpp/src/Symbol.cpp
)

set(GENERATED_SOURCES
	${GENERATED_WL_SOURCES}
	${GENERATED_WL_DATA_SOURCES}
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_SOURCES}
)

set(DOCUMENTATION_NOTEBOOKS
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ParseString.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ParseFile.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ConcreteParseString.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ConcreteParseFile.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/Tutorials/Examples.nb
)

set(BUILT_DOCUMENTATION_NOTEBOOKS
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/Index/_0.cfs
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/Index/segments.gen
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/Index/segments_3
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/ReferencePages/Symbols/ParseString.nb
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/ReferencePages/Symbols/ParseFile.nb
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/ReferencePages/Symbols/ConcreteParseString.nb
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/ReferencePages/Symbols/ConcreteParseFile.nb
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/SpellIndex/_0.cfs
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/SpellIndex/segments.gen
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/SpellIndex/segments_3
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/English/Tutorials/Examples.nb
)





#
# Check WolframKernel
#

# get $VersionNumber
execute_process(
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/VersionNumber.wl
	OUTPUT_VARIABLE VERSION_NUMBER
	OUTPUT_STRIP_TRAILING_WHITESPACE
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

message(STATUS "VERSION_NUMBER: ${VERSION_NUMBER}")

if("${VERSION_NUMBER}" STREQUAL "")
	message(FATAL_ERROR "Wolfram Kernel failed")
endif()

if(NOT ${VERSION_NUMBER} GREATER_EQUAL 1100)
	message(FATAL_ERROR "Wolfram Kernel must be at least version 11.0")
endif()







file(MAKE_DIRECTORY
	${CMAKE_BINARY_DIR}/paclet/${PACLET}
)


#
# Configure WL source files
#
# Use configure_file to create a file-level dependency between input and output
#
foreach(SRC ${WL_SOURCES})
	get_filename_component(BARE_SRC ${SRC} NAME)
	set(CONFIGURED_SRC ${CMAKE_BINARY_DIR}/paclet/${PACLET}/${BARE_SRC})
	configure_file(${SRC}
		${CONFIGURED_SRC}
	)
	list(APPEND CONFIGURED_WL_SOURCES ${CONFIGURED_SRC})
endforeach()

set(PACLET_SOURCES
	# Pack script
	${CMAKE_SOURCE_DIR}/scripts/Pack.wl
	${CONFIGURED_WL_SOURCES}
	${GENERATED_WL_SOURCES}
	# wl-ast executable
	wl-ast
)

if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION_NOTEBOOKS}
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
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/Generate.wl -buildDir ${CMAKE_BINARY_DIR}
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)






#
# Build wl-ast executable
#
add_executable(wl-ast
	${GENERATED_CPP_SOURCES}
	${CPP_SOURCES}
)

target_include_directories(wl-ast
	PRIVATE ${CMAKE_SOURCE_DIR}/AST/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
)

set(MAX_EXPRESSION_DEPTH 100 CACHE STRING "Maximum depth allowed by the parser")
set(MAX_EXPRESSION_BREADTH 1000 CACHE STRING "Maximum breadth allowed by the parser")

message(STATUS "MAX_EXPRESSION_DEPTH: ${MAX_EXPRESSION_DEPTH}")
message(STATUS "MAX_EXPRESSION_BREADTH: ${MAX_EXPRESSION_BREADTH}")

target_compile_definitions(wl-ast
	PRIVATE VERSION_NUMBER=${VERSION_NUMBER}
	PRIVATE MAX_EXPRESSION_DEPTH=${MAX_EXPRESSION_DEPTH}
	PRIVATE MAX_EXPRESSION_BREADTH=${MAX_EXPRESSION_BREADTH}
)

# get $SystemID
execute_process(
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/SystemID.wl
	OUTPUT_VARIABLE SYSTEMID
	OUTPUT_STRIP_TRAILING_WHITESPACE
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)

message(STATUS "SYSTEMID: ${SYSTEMID}")

set_target_properties(wl-ast PROPERTIES
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	RUNTIME_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	RUNTIME_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Release on Multi-configuration generators (VS, Xcode)
	RUNTIME_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(wl-ast
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else()
	target_compile_options(wl-ast
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)







#
# docs target
#

add_custom_target(docs
	DEPENDS
		${BUILT_DOCUMENTATION_NOTEBOOKS}
)

set(DOCS_APPPATH /Applications/Eclipse.app/Contents/Eclipse/configuration/org.eclipse.osgi/525/0/.cp/MathematicaSource/ CACHE PATH "Path of MathematicaSource inside Workbench installation")

message(STATUS "DOCS_APPPATH: ${DOCS_APPPATH}")

set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

message(STATUS "DOCS_JLINKPATH: ${DOCS_JLINKPATH}")


#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION_NOTEBOOKS}
	DEPENDS
		${DOCUMENTATION_NOTEBOOKS}
	COMMAND
		ant -DmathExe=${WOLFRAMKERNEL} -DappPath=${DOCS_APPPATH} -Djlinkpath=${DOCS_JLINKPATH} -DinputDir=${CMAKE_SOURCE_DIR}/AST/Documentation/ -DoutputDir=${CMAKE_BINARY_DIR}/paclet/${PACLET}/Documentation/ -file ${DOCS_APPPATH}/DocumentationBuild/SystemFiles/ant/Build/notebook.xml
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)






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
		${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/Pack.wl -pacletDir ${CMAKE_BINARY_DIR}/paclet/${PACLET}
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)



