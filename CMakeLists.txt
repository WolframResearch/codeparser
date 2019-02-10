
cmake_minimum_required(VERSION 3.0)

project(ast)

set(PACLET "AST")

message(STATUS "PACLET: ${PACLET}")

set(PACLET_VERSION "0.9" CACHE STRING "Paclet Version")

message(STATUS "PACLET_VERSION: ${PACLET_VERSION}")

set(WOLFRAMKERNEL /Applications/Mathematica.app/Contents/MacOS/WolframKernel CACHE FILEPATH "Path to WolframKernel")

message(STATUS "WOLFRAMKERNEL: ${WOLFRAMKERNEL}")

set(BUILD_DOCS OFF CACHE BOOL "Build documentation")

message(STATUS "BUILD_DOCS: ${BUILD_DOCS}")

set(WL_SOURCES
	${CMAKE_SOURCE_DIR}/AST/Abstract.wl
	${CMAKE_SOURCE_DIR}/AST/AST.wl
	${CMAKE_SOURCE_DIR}/AST/Boxes.wl
	${CMAKE_SOURCE_DIR}/AST/DeclarationName.wl
	${CMAKE_SOURCE_DIR}/AST/Node.wl
	${CMAKE_SOURCE_DIR}/AST/Symbol.wl
	${CMAKE_SOURCE_DIR}/AST/ToInputFormString.wl
	${CMAKE_SOURCE_DIR}/AST/ToFullFormString.wl
	${CMAKE_SOURCE_DIR}/AST/Utils.wl
)

set(PACLETINFO_SOURCE
	${CMAKE_SOURCE_DIR}/AST/PacletInfo.m
)

set(WL_DATA_SOURCES
	${CMAKE_SOURCE_DIR}/AST/tables/CommaLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/LongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/NewlineLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/OperatorLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/Precedence.wl
	${CMAKE_SOURCE_DIR}/AST/tables/SpaceLongNames.wl
	${CMAKE_SOURCE_DIR}/AST/tables/TokenEnum.wl
	${CMAKE_SOURCE_DIR}/AST/tables/UninterpretableLongNames.wl
)

set(BUILT_PACLETINFO
	${CMAKE_BINARY_DIR}/paclet/AST/PacletInfo.m
)

set(GENERATED_WL_SOURCES
	# not used right now ${CMAKE_BINARY_DIR}/paclet/${PACLET}/LongNameDefines.wl
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Precedence.wl
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/Token.wl
	${CMAKE_BINARY_DIR}/paclet/${PACLET}/LongNameReplacements.wl
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


set(DOCUMENTATION_NOTEBOOKS
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ParseString.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ParseFile.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ConcreteParseString.nb
	${CMAKE_SOURCE_DIR}/AST/Documentation/English/ReferencePages/Symbols/ConcreteParseFile.nb
)

set(BUILT_DOCUMENTATION
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/Index/_0.cfs
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/Index/segments.gen
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/Index/segments_3
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/ReferencePages/Symbols/ParseString.nb
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/ReferencePages/Symbols/ParseFile.nb
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/ReferencePages/Symbols/ConcreteParseString.nb
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/ReferencePages/Symbols/ConcreteParseFile.nb
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/SpellIndex/_0.cfs
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/SpellIndex/segments.gen
	${CMAKE_BINARY_DIR}/paclet/AST/Documentation/English/SpellIndex/segments_3
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
	${BUILT_PACLETINFO}
	${GENERATED_WL_SOURCES}
	wl-ast-lib
	wl-ast-exe
)

if(BUILD_DOCS)

list(APPEND PACLET_SOURCES
	${BUILT_DOCUMENTATION}
)

endif(BUILD_DOCS)




#
# Build PacletInfo.m
#
# Run the BuildPacletInfo.wl script to build a PacletInfo.m appropriate for including in the paclet
#
add_custom_command(
	OUTPUT
		${BUILT_PACLETINFO}
	DEPENDS
		${CMAKE_SOURCE_DIR}/scripts/BuildPacletInfo.wl
		${PACLETINFO_SOURCE}
	COMMAND ${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/BuildPacletInfo.wl -source ${PACLETINFO_SOURCE} -built ${BUILT_PACLETINFO} -pacletVersion "${PACLET_VERSION}"
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}
)





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

set(CPP_INCLUDES
	${CMAKE_SOURCE_DIR}/AST/cpp/include/ByteDecoder.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/ByteEncoder.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/CharacterDecoder.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/Node.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/Parselet.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/Parser.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/SourceManager.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/SyntaxIssue.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/Tokenizer.h
	${CMAKE_SOURCE_DIR}/AST/cpp/include/Utils.h
)

set(CPP_LIB_SOURCES
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/API.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/ByteDecoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/ByteEncoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/CharacterDecoder.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/Node.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/Parselet.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/Parser.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/SourceManager.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/SyntaxIssue.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/ToInputFormString.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/Tokenizer.cpp
	${CMAKE_SOURCE_DIR}/AST/cpp/src/lib/Utils.cpp
)

add_library(wl-ast-lib SHARED
	${CPP_INCLUDES}
	${CPP_LIB_SOURCES}
	${GENERATED_CPP_INCLUDES}
	${GENERATED_CPP_LIB_SOURCES}
)

target_include_directories(wl-ast-lib
	PRIVATE ${CMAKE_SOURCE_DIR}/AST/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
	PRIVATE ${MATHLINK_INCLUDE_DIR}
	PRIVATE ${WOLFRAMLIBRARY_INCLUDE_DIR}
)

find_library(ML_LIB
	NAMES MLi4
	HINTS ${MATHLINK_LIB_DIR}
)

target_link_libraries(wl-ast-lib ${ML_LIB})

#
# https://community.wolfram.com/groups/-/m/t/610879?sortMsg=Flat
#
target_link_libraries(wl-ast-lib
	#"-framework Foundation"
	"-stdlib=libc++ -framework Foundation -lc++"
)

set(MAX_EXPRESSION_DEPTH 100 CACHE STRING "Maximum depth allowed by the parser")
set(MAX_EXPRESSION_BREADTH 1000 CACHE STRING "Maximum breadth allowed by the parser")

message(STATUS "MAX_EXPRESSION_DEPTH: ${MAX_EXPRESSION_DEPTH}")
message(STATUS "MAX_EXPRESSION_BREADTH: ${MAX_EXPRESSION_BREADTH}")

target_compile_definitions(wl-ast-lib
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

set_target_properties(wl-ast-lib PROPERTIES
	OUTPUT_NAME
		wl-ast
	CXX_STANDARD
		11
	CXX_STANDARD_REQUIRED
		ON
	LIBRARY_OUTPUT_DIRECTORY
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Debug on Multi-configuration generators (VS, Xcode)
	LIBRARY_OUTPUT_DIRECTORY_DEBUG
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
	# make sure that it is not created inside ${SYSTEMID}/Release on Multi-configuration generators (VS, Xcode)
	LIBRARY_OUTPUT_DIRECTORY_RELEASE
		${CMAKE_BINARY_DIR}/paclet/${PACLET}/${PACLET}Resources/${SYSTEMID}
)

#
# Setup warnings
#
if(MSVC)
	target_compile_options(wl-ast-lib
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else()
	target_compile_options(wl-ast-lib
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)





#
# Build executable
#

set(CPP_EXE_SOURCES
	${CMAKE_SOURCE_DIR}/AST/cpp/src/exe/main.cpp
)

add_executable(wl-ast-exe
	${CPP_EXE_SOURCES}
)

target_include_directories(wl-ast-exe
	PRIVATE ${CMAKE_SOURCE_DIR}/AST/cpp/include
	PRIVATE ${CMAKE_BINARY_DIR}/generated/cpp/include
)

target_link_libraries(wl-ast-exe wl-ast-lib)

set_target_properties(wl-ast-exe PROPERTIES
	OUTPUT_NAME
		wl-ast
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
	target_compile_options(wl-ast-exe
		# specify /MT to statically link runtime on Windows
		# This prevents "The program can't start because ucrtbased.dll is missing from your computer" error on Windows 7
		PRIVATE /W3 /EHsc /MT
	)
else()
	target_compile_options(wl-ast-exe
		PRIVATE -Wextra -Wall -Weffc++ -Wno-unused-parameter -Wno-unused-function -Wno-comment
	)
endif(MSVC)











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

set(DOCS_JLINKPATH /Applications/Mathematica.app/Contents/SystemFiles/Links/JLink/ CACHE PATH "Path of JLink")

message(STATUS "DOCS_JLINKPATH: ${DOCS_JLINKPATH}")


#
# build the docs
#
add_custom_command(
	OUTPUT
		${BUILT_DOCUMENTATION}
	DEPENDS
		${DOCUMENTATION_NOTEBOOKS}
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
		${WOLFRAMKERNEL} -noinit -script ${CMAKE_SOURCE_DIR}/scripts/Pack.wl -pacletDir ${CMAKE_BINARY_DIR}/paclet/${PACLET}
	WORKING_DIRECTORY
		${CMAKE_SOURCE_DIR}
)



